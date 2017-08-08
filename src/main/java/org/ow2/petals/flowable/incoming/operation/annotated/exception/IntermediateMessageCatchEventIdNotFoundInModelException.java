/**
 * Copyright (c) 2017 Linagora
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
 * The identifier of the activity associated to a intermediate message catch event is required in the BPMN model
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IntermediateMessageCatchEventIdNotFoundInModelException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = -550620840410922149L;

    private static final String MESSAGE_PATTERN = "The identifier of the activity associated to the intermediate message catch event '%s' is required in the process '%s'";

    /**
     * The message name associated to the intermediate message catch event for which the identifier is missing in the
     * BPMN model
     */
    private final String messageEventName;

    /**
     * The identifier of the process definition containing the invalid intermediate message event activity
     */
    private final String processDefinitionId;

    public IntermediateMessageCatchEventIdNotFoundInModelException(final QName wsdlOperation,
            final String messageEventName, final String processDefinitionId) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, messageEventName, processDefinitionId));
        this.messageEventName = messageEventName;
        this.processDefinitionId = processDefinitionId;
    }

    /**
     * @return The message name associated to the intermediate message catch event for which the identifier is missing
     *         in the BPMN model
     */
    public String getMessageEventName() {
        return this.messageEventName;
    }

    /**
     * @return The identifier of the process definition containing the invalid intermediate message event activity
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }
}
