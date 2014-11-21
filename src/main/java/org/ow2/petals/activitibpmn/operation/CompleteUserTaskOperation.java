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
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPathExpressionException;

import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
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
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param logger
     */
    public CompleteUserTaskOperation(final AnnotatedOperation annotatedOperation, final Logger logger) {
        super(annotatedOperation, logger);
    }

    @Override
    protected String doExecute(final DOMSource domSource, final TaskService taskService,
            final IdentityService identityService, final RuntimeService runtimeService, final String bpmnUserId,
            final Map<String, Object> processVars)
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
        final List<Task> taskList = taskService.createTaskQuery().processInstanceId(processInstanceId)
                .taskDefinitionKey(this.actionId).list();
        if ((taskList == null) || (taskList.size() == 0)) {
            // This error should not occur because this check has been done when deploying the SU
            throw new MessagingException("No tasks: " + this.actionId + " for process instance id: "
                    + processInstanceId + " was found.");
        }
        // Perform the user Task
        try {
            identityService.setAuthenticatedUserId(bpmnUserId);
            taskService.complete(taskList.get(0).getId(), processVars);
        } finally {
            identityService.setAuthenticatedUserId(null);
        }

        return processInstanceId;
    }

    @Override
    public String getAction() {
        return CompleteUserTaskAnnotatedOperation.BPMN_ACTION;
    }

}
