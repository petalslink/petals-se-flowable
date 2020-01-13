/**
 * Copyright (c) 2015-2020 Linagora
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
 * A WSDL annotation is invalid for the operation
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class InvalidAnnotationForOperationException extends InvalidAnnotationException {

    private static final long serialVersionUID = 4807726306653521594L;

    private static final String MESSAGE_PATTERN = "A WSDL annotation is invalid for the binding operation '%s': %s";

    /**
     * Name of the WSDL operation for which an error occurs
     */
    private final QName wsdlOperation;

    public InvalidAnnotationForOperationException(final QName wsdlOperation, final String message) {
        super(String.format(MESSAGE_PATTERN, wsdlOperation, message));
        this.wsdlOperation = wsdlOperation;
    }

    public InvalidAnnotationForOperationException(final QName wsdlOperation, final String message,
            final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, wsdlOperation, message), cause);
        this.wsdlOperation = wsdlOperation;
    }

    /**
     * @return The name of the WSDL operation for which an error occurs
     */
    public QName getWsdlOperation() {
        return this.wsdlOperation;
    }
}
