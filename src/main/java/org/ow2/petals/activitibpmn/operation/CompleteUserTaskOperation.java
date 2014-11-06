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
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;

import org.activiti.bpmn.model.FormProperty;
import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.activiti.engine.task.Task;
import org.ow2.petals.activitibpmn.operation.exception.InvalidAnnotationException;
import org.ow2.petals.activitibpmn.operation.exception.NoProcessIdMappingException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * The operation to complete the user task of process instance
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 * 
 */
public class CompleteUserTaskOperation extends ActivitiOperation {

    public static final String BPMN_ACTION_TYPE = "userTask";

    public CompleteUserTaskOperation(final String processDefinitionId, final String processKey, final String bpmnAction,
            final Properties bpmnProcessId, final Properties bpmnUserId, final Properties bpmnVarInMsg,
            final Properties outMsgBpmnVar, final Properties faultMsgBpmnVar,
            final Map<String, FormProperty> bpmnVarType, final Logger logger) throws InvalidAnnotationException {
        super(processDefinitionId, processKey, bpmnAction, bpmnProcessId, bpmnUserId, bpmnVarInMsg, outMsgBpmnVar,
                faultMsgBpmnVar, bpmnVarType, logger);
    }

    @Override
    protected void verifyParameters(final String processDefinitionId, final String processKey, final String bpmnAction,
            final Properties bpmnProcessId, final Properties bpmnUserId, final Properties bpmnVarInMsg,
            final Properties outMsgBpmnVar, final Properties faultMsgBpmnVar,
            final Map<String, FormProperty> bpmnVarType) throws InvalidAnnotationException {
        
        super.verifyParameters(processDefinitionId, processKey, bpmnAction, bpmnProcessId, bpmnUserId, bpmnVarInMsg,
                outMsgBpmnVar, faultMsgBpmnVar, bpmnVarType);

        // The mapping defining the process instance id is required to complete a user task
        final String processInstanceIdMapping = bpmnProcessId.getProperty("inMsg");
        if (processInstanceIdMapping == null || processInstanceIdMapping.isEmpty()) {
            throw new NoProcessIdMappingException(this);
        }

    }

    @Override
    protected String doExecute(final Document inMsgWsdl, final TaskService taskService,
            final IdentityService identityService, final RuntimeService runtimeService, final String bpmnUserId,
            final Map<String, Object> processVars)
            throws MessagingException {

        // Get the processId
        final String varNameInMsg = this.bpmnProcessId.getProperty("inMsg");
        final String bpmnProcessIdValue;
        final Node varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su",
                varNameInMsg).item(0);
        if (varNode == null) {
            throw new MessagingException(
                    "The bpmnProcessId is mandatory and must be given through the message variable: " + varNameInMsg
                            + " for the userTask: " + bpmnAction + " of process: " + processDefinitionId + " !");
        } else {
            bpmnProcessIdValue = varNode.getTextContent().trim();
        }

        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("bpmnProcessId => InMsg = " + varNameInMsg + " - value = " + bpmnProcessIdValue);
        }

        // Get the task
        final List<Task> taskList = taskService.createTaskQuery().processInstanceId(bpmnProcessIdValue)
                .taskDefinitionKey(bpmnAction).list();
        if ((taskList == null) || (taskList.size() == 0)) {
            throw new MessagingException("No tasks: " + bpmnAction + " for processInstance: " + bpmnProcessIdValue
                    + " was found.");
        }
        // Perform the user Task
        try {
            identityService.setAuthenticatedUserId(bpmnUserId);
            taskService.complete(taskList.get(0).getId(), processVars);
        } finally {
            identityService.setAuthenticatedUserId(null);
        }

        return bpmnProcessIdValue;
    }

    @Override
    public String getName() {
        return BPMN_ACTION_TYPE;
    }

}
