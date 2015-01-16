/**
 * Copyright (c) 2015 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.incoming.operation;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPathExpressionException;

import org.activiti.engine.HistoryService;
import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.activiti.engine.history.HistoricTaskInstance;
import org.activiti.engine.task.Task;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.AnnotatedOperation;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.CompleteUserTaskAnnotatedOperation;
import org.ow2.petals.activitibpmn.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.activitibpmn.incoming.operation.exception.OperationProcessingException;
import org.ow2.petals.activitibpmn.incoming.operation.exception.OperationProcessingFault;
import org.ow2.petals.activitibpmn.incoming.operation.exception.ProcessInstanceNotFoundException;
import org.ow2.petals.activitibpmn.incoming.operation.exception.TaskCompletedException;
import org.ow2.petals.activitibpmn.incoming.operation.exception.UnexpectedUserException;
import org.ow2.petals.activitibpmn.utils.XslUtils;

/**
 * The operation to complete the user task of process instance
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 * 
 */
public class CompleteUserTaskOperation extends ActivitiOperation {

    /**
     * The identity service of the BPMN engine
     */
    private final IdentityService identityService;

    /**
     * The runtime service of the BPMN engine
     */
    private final RuntimeService runtimeService;

    /**
     * The task service of the BPMN engine
     */
    private final TaskService taskService;

    /**
     * The history service of the BPMN engine
     */
    private final HistoryService historyService;

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param logger
     */
    public CompleteUserTaskOperation(final AnnotatedOperation annotatedOperation, final TaskService taskService,
            final IdentityService identityService, final HistoryService historyService,
            final RuntimeService runtimeService, final Logger logger) {
        super(annotatedOperation, logger);
        this.identityService = identityService;
        this.taskService = taskService;
        this.historyService = historyService;
        this.runtimeService = runtimeService;
    }

    @Override
    protected void doExecute(final DOMSource domSource, final String bpmnUserId, final Map<String, Object> processVars,
            final Map<QName, String> outputNamedValues) throws OperationProcessingException {

        // Get the process instance identifier
        final String processInstanceId;
        try {
            processInstanceId = this.proccesInstanceIdXPathExpr.evaluate(domSource);
            if (processInstanceId == null || processInstanceId.trim().isEmpty()) {
                throw new NoProcessInstanceIdValueException(this.wsdlOperationName);
            }

            if (this.logger.isLoggable(Level.FINE)) {
                this.logger.fine("Process instance identifier value: " + processInstanceId);
            }
        } catch (final XPathExpressionException e) {
            throw new OperationProcessingException(this.wsdlOperationName, e);
        }

        // Get the task
        // TODO: We should select only task assigned to the user id
        final List<Task> taskList = this.taskService.createTaskQuery().processInstanceId(processInstanceId)
                .taskDefinitionKey(this.actionId).list();
        if ((taskList == null) || (taskList.size() == 0)) {
            this.investigateMissingTask(processInstanceId, bpmnUserId);
        }

        // Perform the user Task
        final Task taskToComplete = taskList.get(0);
        try {
            this.identityService.setAuthenticatedUserId(bpmnUserId);
            this.taskService.complete(taskToComplete.getId(), processVars);
        } finally {
            this.identityService.setAuthenticatedUserId(null);
        }

        // To prepare the output response, we add named value dedicated to this operation:
        // - task local variables
        // - process instance variables
        // - the identifier of the process instance
        // As the task is completed, it is retrieved from the history.
        // TODO: Are task local variables different from the provided variables 'processVars' ? If no, don't retrieve
        // them and use directly 'processVars'
        final HistoricTaskInstance executedTask = this.historyService.createHistoricTaskInstanceQuery().finished()
                .taskId(taskToComplete.getId()).includeProcessVariables().includeTaskLocalVariables().singleResult();
        if (executedTask == null) {
            // This exception should not occur
            throw new OperationProcessingException(this.wsdlOperationName, String.format(
                    "The just completed task '%s' is not found in the history for the process instance '%s'.",
                    this.actionId,
                    processInstanceId));
        }
        for (final Entry<String, Object> processVariable : executedTask.getTaskLocalVariables().entrySet()) {
            // TODO: Create unit test for these task local variables
            outputNamedValues.put(
                    new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_TASK_PARAMS, processVariable.getKey()),
                    XslUtils.convertBpmnVariableValueToXslParam(processVariable.getValue()));

        }
        for (final Entry<String, Object> processVariable : executedTask.getProcessVariables().entrySet()) {
            outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_PROCESS_INSTANCE_PARAMS,
                    processVariable.getKey()), XslUtils.convertBpmnVariableValueToXslParam(processVariable.getValue()));

        }
        outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS,
                SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID), processInstanceId);
        outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS,
                SCHEMA_OUTPUT_XSLT_PARAM_USER_ID), bpmnUserId);
    }

    /**
     * <p>
     * Investigate why no active task was found for the process instance identifier.
     * </p>
     * <p>
     * Several possible causes:</ul>
     * <li>the process instance id does not exist,</li>
     * <li>the process instance is finished,</li>
     * <li>the task was completed with a previous service call,</li>
     * <li>the task is assigned to another user.</li>
     * </ul>
     * </p>
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param bpmnUserId
     *            The user identifier used for the task assignee
     * @throws OperationProcessingFault
     *             The cause to have not found an active task for the process instance identifier
     * @throws OperationProcessingException
     *             No cause found.
     */
    private void investigateMissingTask(final String processInstanceId, final String bpmnUserId)
            throws OperationProcessingFault, OperationProcessingException {

        if (this.historyService.createHistoricProcessInstanceQuery().finished().processInstanceId(processInstanceId)
                .singleResult() != null) {
            // The process instance is finished, so the task is finished !
            throw new TaskCompletedException(this.wsdlOperationName, processInstanceId, this.actionId);
        } else if (this.runtimeService.createProcessInstanceQuery().processInstanceId(processInstanceId).singleResult() == null) {
            // No active process instance found for the process instance identifier
            throw new ProcessInstanceNotFoundException(this.wsdlOperationName, processInstanceId);
        } else if (this.historyService.createHistoricTaskInstanceQuery().finished()
                .processInstanceId(processInstanceId).taskDefinitionKey(this.actionId).singleResult() != null) {
            // The task of the active process instance is finished
            throw new TaskCompletedException(this.wsdlOperationName, processInstanceId, this.actionId);
        } else if (this.taskService.createTaskQuery().processInstanceId(processInstanceId)
                .taskDefinitionKey(this.actionId).singleResult() != null) {
            // The task assignee is not the expected one
            // TODO: Add a unit test
            throw new UnexpectedUserException(this.wsdlOperationName, processInstanceId, this.actionId, bpmnUserId);
        } else {
            // This error case should not occur. If this error occurs, it is likely that an business error case is
            // missing from the above conditions
            throw new OperationProcessingException(wsdlOperationName, String.format(
                    "The task '%s' is not a current user task to complete for the process instance '%s'.",
                    this.actionId, processInstanceId));
        }
    }

    @Override
    public String getAction() {
        return CompleteUserTaskAnnotatedOperation.BPMN_ACTION;
    }

}
