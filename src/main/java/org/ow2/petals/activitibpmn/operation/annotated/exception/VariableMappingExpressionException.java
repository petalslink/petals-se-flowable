/**
 * Copyright (c) 2014-2015 Linagora
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

import javax.xml.xpath.XPathExpressionException;

/**
 * The expression defining a variable is invalid.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class VariableMappingExpressionException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 718914773820362365L;

    private static final String MESSAGE_PATTERN = "The mapping defining the expression of the variable '%s' is invalid: %s";

    /**
     * Name of the variable having an invalid expression
     */
    private final String variableName;

    public VariableMappingExpressionException(final String wsdlOperationName, final String variableName,
            final XPathExpressionException cause) {
        super(wsdlOperationName, String.format(MESSAGE_PATTERN, variableName, cause.getMessage()), cause);
        this.variableName = variableName;
    }

    /**
     * @return The name of the variable having an invalid expression
     */
    public String getVariableName() {
        return this.variableName;
    }

}
