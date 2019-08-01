/**
 * Copyright (c) 2018-2020 Linagora
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
 * A BPMN annotation is missing to declared a variable name
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class VariableTypeRequiredException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = -7120471251778442787L;

    private static final String MESSAGE_PATTERN = "A BPMN variable is declared for which the type must be defined: %s";

    public VariableTypeRequiredException(final QName wsdlOperation, final String variableName) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, variableName));
    }

}
