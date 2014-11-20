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

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.xml.transform.dom.DOMSource;

import org.activiti.bpmn.model.FormProperty;
import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.activiti.engine.runtime.ProcessInstance;
import org.ow2.petals.activitibpmn.operation.annotated.AnnotatedOperation;
import org.ow2.petals.activitibpmn.operation.annotated.StartEventAnnotatedOperation;

/**
 * The operation to create a new instance of a process
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 * 
 */
public class StartEventOperation extends ActivitiOperation {

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param processDefinitionId
     *            The process definition identifier to associate to the operation to create
     * @param bpmnVarType
     * @param logger
     */
    public StartEventOperation(final AnnotatedOperation annotatedOperation, final String processDefinitionId,
            final Map<String, FormProperty> bpmnVarType, final Logger logger) {
        super(annotatedOperation, processDefinitionId, bpmnVarType, logger);
    }

    @Override
    public String getBpmnActionType() {
        return StartEventAnnotatedOperation.BPMN_ACTION_TYPE;
    }

    @Override
    protected String doExecute(final DOMSource domSource, final TaskService taskService,
            final IdentityService identityService, final RuntimeService runtimeService, final String bpmnUserId,
            final Map<String, Object> processVars) throws MessagingException {

        // Start a new process instance
        // TODO Set the CategoryId (not automatically done, but automatically done for tenant_id ?)
        final String bpmnProcessIdValue;
        try {
            // TODO: How this works on concurrent requests. I think that it is not thread-safe ?
            identityService.setAuthenticatedUserId(bpmnUserId);
            // TODO: How to manage process definition having several start event ?
            final ProcessInstance processInstance = runtimeService.startProcessInstanceById(this.processDefinitionId,
                    processVars);
            bpmnProcessIdValue = processInstance.getId();
        } finally {
            identityService.setAuthenticatedUserId(null);
        }

        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("*** NEW PROCESS INSTANCE started,  processId = " + bpmnProcessIdValue);
        }

        return bpmnProcessIdValue;
    }

}
