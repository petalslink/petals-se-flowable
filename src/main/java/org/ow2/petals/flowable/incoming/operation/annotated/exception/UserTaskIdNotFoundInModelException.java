/**
 * Copyright (c) 2017-2019 Linagora
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
package org.ow2.petals.flowable.incoming.operation.annotated.exception;

import javax.xml.namespace.QName;

/**
 * The annotation defining the user task identifier is required for the given WSDL binding operation.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class UserTaskIdNotFoundInModelException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 6574233323910052086L;

    private static final String MESSAGE_PATTERN = "The annotation defining the user task identifier is set to a value '%s' that does not exist into the process definition '%s'";

    /**
     * The user task identifier that does not exist in the BPMN model
     */
    private final String userTaskId;

    /**
     * The process definition identifier for which the user task identifier is not found
     */
    private final String processDefinitionId;

    public UserTaskIdNotFoundInModelException(final QName wsdlOperation, final String userTaskId,
            final String processDefinitionId) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, userTaskId, processDefinitionId));
        this.userTaskId = userTaskId;
        this.processDefinitionId = processDefinitionId;
    }

    /**
     * @return The user task identifier that does not exist in the BPMN model
     */
    public String getUserTaskId() {
        return this.userTaskId;
    }

    /**
     * @return The process definition identifier for which the action identifier is not found
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }
}
