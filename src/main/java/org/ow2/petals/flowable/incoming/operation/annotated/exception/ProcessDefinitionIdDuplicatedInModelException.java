/**
 * Copyright (c) 2015-2025 Linagora
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
 * The process definition identifier declared with annotations exists several times in the BPMN models.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ProcessDefinitionIdDuplicatedInModelException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 5116023245724275981L;

    private static final String MESSAGE_PATTERN = "The process definition identifier '%s' declared with annotations exists several times into the embedded process definitions";

    /**
     * The process definition identifier that is duplicated
     */
    private final String processDefinitionId;

    public ProcessDefinitionIdDuplicatedInModelException(final QName wsdlOperation,
            final String processDefinitionId) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, processDefinitionId));
        this.processDefinitionId = processDefinitionId;
    }

    /**
     * @return The process definition identifier that is duplicated
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }
}
