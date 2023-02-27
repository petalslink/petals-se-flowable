/**
 * Copyright (c) 2019-2023 Linagora
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
 * The virtual root is required for variable of type 'json'.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class RequiredVirtualRootException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = -3720502111554795729L;

    private static final String MESSAGE_PATTERN = "The virtual root attribute is required for variable '%s' because of its type 'json'.";

    /**
     * The name of the variable for which the virtual root is missing.
     */
    private final String variableName;

    public RequiredVirtualRootException(final QName wsdlOperation, final String variableName) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, variableName));
        this.variableName = variableName;
    }

    public String getVariableName() {
        return this.variableName;
    }

}
