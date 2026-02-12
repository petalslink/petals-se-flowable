/**
 * Copyright (c) 2019-2026 Linagora
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
 * Prefix missing in a namespace mapping definition of a JSON variable.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class VariableJsonNsMappingPrefixMissingException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 1482289738880650277L;

    private static final String MESSAGE_PATTERN = "The prefix of the namespace mapping definition of the JSON variable '%s' is missing.";

    /**
     * Name of the variable for which the prefix of a namespace mapping definition is missing.
     */
    private final String variableName;

    public VariableJsonNsMappingPrefixMissingException(final QName wsdlOperation, final String variableName) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, variableName));
        this.variableName = variableName;
    }

    /**
     * @return The name of the variable for which the prefix of a namespace mapping definition is missing.
     */
    public String getVariableName() {
        return this.variableName;
    }

}
