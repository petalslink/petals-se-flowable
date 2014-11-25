/**
 * Copyright (c) 2014 Linagora
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
package org.ow2.petals.activitibpmn.operation;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPathExpressionException;

import org.activiti.engine.HistoryService;
import org.activiti.engine.IdentityService;
import org.activiti.engine.TaskService;
import org.activiti.engine.history.HistoricTaskInstance;
import org.activiti.engine.task.Task;
import org.ow2.petals.activitibpmn.operation.annotated.AnnotatedOperation;
import org.ow2.petals.activitibpmn.operation.annotated.CompleteUserTaskAnnotatedOperation;
import org.ow2.petals.activitibpmn.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.activitibpmn.operation.exception.OperationProcessingException;

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
            final IdentityService identityService, final HistoryService historyService, final Logger logger) {
        super(annotatedOperation, logger);
        this.identityService = identityService;
        this.taskService = taskService;
        this.historyService = historyService;
    }

    @Override
    protected void doExecute(final DOMSource domSource, final String bpmnUserId,
            final Map<String, Object> processVars, final Map<QName, String> outputNamedValues)
            throws MessagingException {

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
        final List<Task> taskList = this.taskService.createTaskQuery().processInstanceId(processInstanceId)
                .taskDefinitionKey(this.actionId).list();
        if ((taskList == null) || (taskList.size() == 0)) {
            throw new MessagingException(String.format(
                    "The task '%s' is not a current user task to complete for the process instance '%s'.",
                    this.actionId, processInstanceId));
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
        final HistoricTaskInstance executedTask = this.historyService.createHistoricTaskInstanceQuery()
                .taskId(taskToComplete.getId()).includeProcessVariables().includeTaskLocalVariables().singleResult();
        if (executedTask == null) {
            throw new MessagingException(String.format(
                    "The just completed task '%s' is not found in the history for the process instance '%s'.",
                    this.actionId,
                    processInstanceId));
        }
        for (final Entry<String, Object> processVariable : executedTask.getTaskLocalVariables().entrySet()) {
            // TODO: Create unit test for these task local variables
            outputNamedValues.put(
                    new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_TASK_PARAMS, processVariable.getKey()),
                    this.convertBpmnVariableValueToXslParam(processVariable.getValue()));

        }
        for (final Entry<String, Object> processVariable : executedTask.getProcessVariables().entrySet()) {
            outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_PROCESS_INSTANCE_PARAMS,
                    processVariable.getKey()), this.convertBpmnVariableValueToXslParam(processVariable.getValue()));

        }
        outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS,
                SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID), processInstanceId);
        outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS,
                SCHEMA_OUTPUT_XSLT_PARAM_USER_ID), bpmnUserId);
    }

    @Override
    public String getAction() {
        return CompleteUserTaskAnnotatedOperation.BPMN_ACTION;
    }

}
