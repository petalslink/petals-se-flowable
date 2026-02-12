/**
 * Copyright (c) 2015-2026 Linagora
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
 * A variable declared with annotations does not exist in the BPMN model.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class VariableNotFoundInModelException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 7045524166630914842L;

    private static final String MESSAGE_PATTERN = "The variable '%s' declared with annotations does not exist into the process definition '%s'";

    /**
     * The name of the variable that does not exist
     */
    private final String variableName;

    /**
     * The process definition identifier for which the variable is not found
     */
    private final String processDefinitionId;

    public VariableNotFoundInModelException(final QName wsdlOperation, final String variableName,
            final String processDefinitionId) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, variableName, processDefinitionId));
        this.variableName = variableName;
        this.processDefinitionId = processDefinitionId;
    }

    /**
     * @return The name of the variable that does not exist
     */
    public String getVariableName() {
        return this.variableName;
    }

    /**
     * @return The process definition identifier for which the variable is not found
     */
    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }

}
