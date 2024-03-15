/**
 * Copyright (c) 2015-2024 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.incoming.operation;

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_STEP_ID;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.flowable.engine.HistoryService;
import org.flowable.engine.IdentityService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.TaskService;
import org.flowable.task.api.Task;
import org.flowable.task.api.history.HistoricTaskInstance;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.commons.log.PetalsExecutionContext;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.flowable.incoming.operation.annotated.CompleteUserTaskAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingFault;
import org.ow2.petals.flowable.incoming.operation.exception.ProcessInstanceNotFoundException;
import org.ow2.petals.flowable.incoming.operation.exception.TaskCompletedException;
import org.ow2.petals.flowable.incoming.operation.exception.UnexpectedUserException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableUnsupportedTypeException;
import org.ow2.petals.flowable.utils.XslUtils;
import org.w3c.dom.Document;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The operation to complete the user task of process instance
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 * 
 */
public class CompleteUserTaskOperation extends FlowableOperation {

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
     * The compiled XPath expression of the process instance identifier placeholder
     */
    private final XPathExpression proccesInstanceIdXPathExpr;

    /**
     * The task identifier on which the action must be realize on the BPMN process side
     */
    private final String userTaskId;

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param jacksonObjectMapper
     *            String to JSON converter of Jackson library.
     * @param logger
     * @throws VariableUnsupportedTypeException
     *             A variable definition contains an unsupported type.
     */
    public CompleteUserTaskOperation(final CompleteUserTaskAnnotatedOperation annotatedOperation,
            final TaskService taskService, final IdentityService identityService, final HistoryService historyService,
            final RuntimeService runtimeService, final ObjectMapper jacksonObjectMapper, final Logger logger)
            throws VariableUnsupportedTypeException {
        super(annotatedOperation, annotatedOperation.getOutputTemplate(), jacksonObjectMapper, logger);
        this.identityService = identityService;
        this.taskService = taskService;
        this.historyService = historyService;
        this.runtimeService = runtimeService;
        this.proccesInstanceIdXPathExpr = annotatedOperation.getProcessInstanceIdHolder();
        this.userTaskId = annotatedOperation.getUserTaskId();
    }

    @Override
    protected void doExecute(final Document incomingPayload, final String bpmnUserId,
            final Map<String, Object> taskVars, final Map<QName, String> outputNamedValues, final Exchange exchange,
            final boolean isFlowTracingEnabled) throws OperationProcessingException {

        // Get the process instance identifier
        final String processInstanceId;
        try {
            synchronized (this.proccesInstanceIdXPathExpr) {
                processInstanceId = this.proccesInstanceIdXPathExpr.evaluate(incomingPayload);
            }
            if (processInstanceId == null || processInstanceId.trim().isEmpty()) {
                throw new NoProcessInstanceIdValueException(this.wsdlOperation);
            }

            if (this.logger.isLoggable(Level.FINE)) {
                this.logger.fine("Process instance identifier value: " + processInstanceId);
            }
        } catch (final XPathExpressionException e) {
            throw new OperationProcessingException(this.wsdlOperation, e);
        }

        // Get the task
        final List<Task> taskList = this.taskService.createTaskQuery().processInstanceId(processInstanceId)
                .taskDefinitionKey(this.userTaskId).taskCandidateOrAssigned(bpmnUserId).list();
        if ((taskList == null) || (taskList.isEmpty())) {
            throw this.investigateMissingTask(processInstanceId, bpmnUserId);
        }
        final Task taskToComplete = taskList.get(0);
        final String taskId = taskToComplete.getId();

        // Set flow attributes as task local variables that will be used by Flowable event listener to generate a MONIT
        // trace
        final FlowAttributes exchangeFlowAttibutes = PetalsExecutionContext.getFlowAttributes();
        final Map<String, Object> taskLocalVariables = new HashMap<>(2);
        taskLocalVariables.put(VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID, exchangeFlowAttibutes.getFlowInstanceId());
        taskLocalVariables.put(VAR_PETALS_CORRELATED_FLOW_STEP_ID, exchangeFlowAttibutes.getFlowStepId());
        this.taskService.setVariablesLocal(taskToComplete.getId(), taskLocalVariables);

        // Before to complete the task, we set explicitly its assignee. It is not done by Flowable engine when
        // completing the task.
        this.taskService.setAssignee(taskToComplete.getId(), bpmnUserId);

        // We complete the task
        try {
            this.identityService.setAuthenticatedUserId(bpmnUserId);
            this.taskService.complete(taskId, taskVars);
        } finally {
            this.identityService.setAuthenticatedUserId(null);
        }

        // To prepare the output response, we add named value dedicated to this operation:
        // - task local variables
        // - process instance variables
        // - the identifier of the process instance
        // As the task is completed, it is retrieved from the history.
        // TODO: Are task local variables different from the provided variables 'processVars' ? If no, don't
        // retrieve them and use directly 'processVars'
        final HistoricTaskInstance executedTask = this.historyService.createHistoricTaskInstanceQuery().finished()
                .taskId(taskToComplete.getId()).includeProcessVariables().includeTaskLocalVariables().singleResult();
        if (executedTask == null) {
            // This exception should not occur
            throw new OperationProcessingException(this.wsdlOperation,
                    String.format(
                            "The just completed task '%s' is not found in the history for the process instance '%s'.",
                            this.userTaskId, processInstanceId));
        }
        for (final Entry<String, Object> processVariable : executedTask.getTaskLocalVariables().entrySet()) {
            // TODO: Create unit test for these task local variables
            outputNamedValues.put(new QName(FlowableOperation.SCHEMA_OUTPUT_XSLT_TASK_PARAMS, processVariable.getKey()),
                    XslUtils.convertBpmnVariableValueToXslParam(processVariable.getValue()));

        }
        for (final Entry<String, Object> processVariable : executedTask.getProcessVariables().entrySet()) {
            outputNamedValues.put(
                    new QName(FlowableOperation.SCHEMA_OUTPUT_XSLT_PROCESS_INSTANCE_PARAMS, processVariable.getKey()),
                    XslUtils.convertBpmnVariableValueToXslParam(processVariable.getValue()));

        }
        outputNamedValues.put(new QName(FlowableOperation.SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS,
                SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID), processInstanceId);
        outputNamedValues.put(
                new QName(FlowableOperation.SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS, SCHEMA_OUTPUT_XSLT_PARAM_USER_ID),
                bpmnUserId);
    }

    /**
     * <p>
     * Investigate why no active task was found for the process instance identifier.
     * </p>
     * <p>
     * Several possible causes:
     * </ul>
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
     *             The cause why no active task was found for the process instance identifier
     * @throws OperationProcessingException
     *             No cause found.
     */
    private OperationProcessingException investigateMissingTask(final String processInstanceId,
            final String bpmnUserId) {

        if (this.historyService.createHistoricProcessInstanceQuery().finished().processInstanceId(processInstanceId)
                .singleResult() != null) {
            // The process instance is finished, so the task is finished !
            return new TaskCompletedException(this.wsdlOperation, processInstanceId, this.userTaskId);
        } else if (this.runtimeService.createProcessInstanceQuery().processInstanceId(processInstanceId)
                .singleResult() == null) {
            // No active process instance found for the process instance identifier
            return new ProcessInstanceNotFoundException(this.wsdlOperation, processInstanceId);
        } else if (this.historyService.createHistoricTaskInstanceQuery().finished().processInstanceId(processInstanceId)
                .taskDefinitionKey(this.userTaskId).singleResult() != null) {
            // The task of the active process instance is finished
            return new TaskCompletedException(this.wsdlOperation, processInstanceId, this.userTaskId);
        } else if (this.taskService.createTaskQuery().processInstanceId(processInstanceId)
                .taskDefinitionKey(this.userTaskId).singleResult() != null) {
            // The task assignee is not the expected one
            // TODO: Add a unit test
            return new UnexpectedUserException(this.wsdlOperation, processInstanceId, this.userTaskId, bpmnUserId);
        } else {
            // This error case should not occur. If this error occurs, it is likely that an business error case is
            // missing from the above conditions
            return new OperationProcessingException(wsdlOperation,
                    String.format("The task '%s' is not a current user task to complete for the process instance '%s'.",
                            this.userTaskId, processInstanceId));
        }
    }

    @Override
    public String getAction() {
        return CompleteUserTaskAnnotatedOperation.BPMN_ACTION;
    }

    @Override
    protected void logFlowableOperation() {
        super.logFlowableOperation();
        this.logger.fine("Flowable User Task Id = " + this.userTaskId);
    }

}
