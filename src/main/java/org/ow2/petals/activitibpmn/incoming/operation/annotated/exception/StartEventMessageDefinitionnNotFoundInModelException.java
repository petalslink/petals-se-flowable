/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.activitibpmn.incoming.operation.annotated.exception;

import javax.xml.namespace.QName;

/**
 * The message definition associated to the annotation defining the start event message name through the message
 * reference attribute is not found in the BPM model for the given WSDL binding operation.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class StartEventMessageDefinitionnNotFoundInModelException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 6859973170418500203L;

    private static final String MESSAGE_PATTERN = "The message definition '%s' associated to the annotation defining the start event message name '%s' does not exist into the process definition '%s'";

    /**
     * The start event message name for which the message definition identifier is not found
     */
    private final String startEventMessageName;

    /**
     * The message definition identifier that does not exist in the BPMN model.
     */
    private final String messageDefinitionId;

    /**
     * The process definition identifier for which the message definition identifier is not found.
     */
    private final String processDefinitionId;

    public StartEventMessageDefinitionnNotFoundInModelException(final QName wsdlOperation,
            final String startEventMessageName, final String messageDefinitionId, final String processDefinitionId) {
        super(wsdlOperation,
                String.format(MESSAGE_PATTERN, messageDefinitionId, startEventMessageName, processDefinitionId));
        this.startEventMessageName = startEventMessageName;
        this.messageDefinitionId = messageDefinitionId;
        this.processDefinitionId = processDefinitionId;
    }

    /**
     * @return The start event message name for which the message definition identifier is not found
     */
    public String getStartEventMessageName() {
        return this.startEventMessageName;
    }

    /**
     * @return The process definition identifier for which the message definition identifier is not found
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }

    /**
     * @return The message definition identifier that does not exist in the BPMN model.
     */
    public String getMessageDefinitionId() {
        return this.messageDefinitionId;
    }
}
