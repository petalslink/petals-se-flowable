/**
 * Copyright (c) 2015 Linagora
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
package org.ow2.petals.activitibpmn.incoming.operation.exception;

import javax.jbi.messaging.MessagingException;

import org.ow2.petals.activitibpmn.incoming.operation.ActivitiOperation;

/**
 * An error occurs during the processing of an {@link ActivitiOperation}.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class OperationProcessingException extends MessagingException {

    private static final long serialVersionUID = 4441743046434029782L;

    private static final String MESSAGE_PATTERN = "An error occurs processing the operation '%s': %s";

    /**
     * Name of the WSDL operation associated to the {@link ActivitiOperation} for which an error occurs
     */
    private final String wsdlOperationName;

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation associated to the {@link ActivitiOperation} for which an error occurs
     * @param message
     *            The error message
     */
    public OperationProcessingException(final String wsdlOperationName, final String message) {
        super(String.format(MESSAGE_PATTERN, wsdlOperationName, message));
        this.wsdlOperationName = wsdlOperationName;
    }

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation associated to the {@link ActivitiOperation} for which an error occurs
     * @param cause
     *            The error cause
     */
    public OperationProcessingException(final String wsdlOperationName, final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, wsdlOperationName, cause.getMessage()), cause);
        this.wsdlOperationName = wsdlOperationName;
    }

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation associated to the {@link ActivitiOperation} for which an error occurs
     * @param message
     *            The error message
     * @param cause
     *            The error cause
     */
    public OperationProcessingException(final String wsdlOperationName, final String message, final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, wsdlOperationName, message), cause);
        this.wsdlOperationName = wsdlOperationName;
    }

    /**
     * @return The name of the WSDL operation associated to the {@link ActivitiOperation} for which an error occurs
     */
    public String getWsdlOperationName() {
        return this.wsdlOperationName;
    }
}
