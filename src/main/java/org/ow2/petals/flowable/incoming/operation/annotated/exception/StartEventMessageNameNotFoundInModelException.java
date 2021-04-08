/**
 * Copyright (c) 2015-2021 Linagora
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
 * The annotation defining the start event message name is required for the given WSDL binding operation.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class StartEventMessageNameNotFoundInModelException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 6859973170418500203L;

    private static final String MESSAGE_PATTERN = "The annotation defining the start event message name is set to a value '%s' that does not exist into the process definition '%s'";

    /**
     * The start event message name that does not exist in the BPMN model
     */
    private final String startEventMessageName;

    /**
     * The process definition identifier for which the start event message name is not found
     */
    private final String processDefinitionId;

    public StartEventMessageNameNotFoundInModelException(final QName wsdlOperation, final String startEventMessageName,
            final String processDefinitionId) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, startEventMessageName, processDefinitionId));
        this.startEventMessageName = startEventMessageName;
        this.processDefinitionId = processDefinitionId;
    }

    /**
     * @return The start event message name that does not exist in the BPMN model
     */
    public String getStartEventMessageName() {
        return this.startEventMessageName;
    }

    /**
     * @return The process definition identifier for which the start event message name is not found
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }
}
