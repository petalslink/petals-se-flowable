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
package org.ow2.petals.activitibpmn.operation.annotated.exception;

import org.ow2.petals.activitibpmn.operation.CompleteUserTaskOperation;
import org.ow2.petals.activitibpmn.operation.StartEventOperation;

/**
 * For the operations {@link CompleteUserTaskOperation} and {@link StartEventOperation}, the mapping defining the action
 * identifier identifier is required.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ActionIdNotFoundInModelException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = -3175460203224153888L;

    private static final String MESSAGE_PATTERN = "The mapping defining the action identifier is set to a value '%s' that does not exist into the process definition '%s'";

    /**
     * The action identifier that does not exist in the BPMN model
     */
    private final String actionId;

    /**
     * The process definition identifier for which the action identifier is not found
     */
    private final String processDefinitionId;

    public ActionIdNotFoundInModelException(final String wsdlOperationName, final String actionId,
            final String processDefinitionId) {
        super(wsdlOperationName, String.format(MESSAGE_PATTERN, actionId, processDefinitionId));
        this.actionId = actionId;
        this.processDefinitionId = processDefinitionId;
    }

    /**
     * @return The action identifier that does not exist in the BPMN model
     */
    public String getActionId() {
        return this.actionId;
    }

    /**
     * @return The process definition identifier for which the action identifier is not found
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }
}
