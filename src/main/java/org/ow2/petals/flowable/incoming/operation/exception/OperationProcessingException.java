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
package org.ow2.petals.flowable.incoming.operation.exception;

import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;

import org.ow2.petals.flowable.incoming.operation.FlowableOperation;

/**
 * An error occurs during the processing of an {@link FlowableOperation} that must be returned as terchnical error.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class OperationProcessingException extends MessagingException {

    private static final long serialVersionUID = 4441743046434029782L;

    private static final String MESSAGE_PATTERN = "An error occurs processing the operation '%s': %s";

    /**
     * Name of the WSDL operation associated to the {@link FlowableOperation} for which an error occurs
     */
    private final QName wsdlOperation;

    /**
     * 
     * @param wsdlOperation
     *            The WSDL operation associated to the {@link FlowableOperation} for which an error occurs
     * @param message
     *            The error message
     */
    public OperationProcessingException(final QName wsdlOperation, final String message) {
        super(String.format(MESSAGE_PATTERN, wsdlOperation, message));
        this.wsdlOperation = wsdlOperation;
    }

    /**
     * 
     * @param wsdlOperation
     *            The WSDL operation associated to the {@link FlowableOperation} for which an error occurs
     * @param cause
     *            The error cause
     */
    public OperationProcessingException(final QName wsdlOperation, final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, wsdlOperation, cause.getMessage()), cause);
        this.wsdlOperation = wsdlOperation;
    }

    /**
     * 
     * @param wsdlOperation
     *            The WSDL operation associated to the {@link FlowableOperation} for which an error occurs
     * @param message
     *            The error message
     * @param cause
     *            The error cause
     */
    public OperationProcessingException(final QName wsdlOperation, final String message, final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, wsdlOperation, message), cause);
        this.wsdlOperation = wsdlOperation;
    }

    /**
     * @return The name of the WSDL operation associated to the {@link FlowableOperation} for which an error occurs
     */
    public QName getWsdlOperation() {
        return this.wsdlOperation;
    }
}
