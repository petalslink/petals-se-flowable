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

import java.util.Map;

import javax.xml.namespace.QName;

import org.ow2.petals.activitibpmn.incoming.operation.ActivitiOperation;

/**
 * An error occurs during the processing of an {@link ActivitiOperation}.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class OperationProcessingFault extends OperationProcessingException {

    private static final long serialVersionUID = 5533666375185014637L;

    /**
     * 
     * @param wsdlOperation
     *            The WSDL operation associated to the {@link ActivitiOperation} for which an error occurs
     * @param message
     *            The error message
     */
    public OperationProcessingFault(final QName wsdlOperation, final String message) {
        super(wsdlOperation, message);
    }

    /**
     * 
     * @param wsdlOperation
     *            The WSDL operation associated to the {@link ActivitiOperation} for which an error occurs
     * @param cause
     *            The error cause
     */
    public OperationProcessingFault(final QName wsdlOperation, final Throwable cause) {
        super(wsdlOperation, cause);
    }

    /**
     * 
     * @param wsdlOperation
     *            The WSDL operation associated to the {@link ActivitiOperation} for which an error occurs
     * @param message
     *            The error message
     * @param cause
     *            The error cause
     */
    public OperationProcessingFault(final QName wsdlOperation, final String message, final Throwable cause) {
        super(wsdlOperation, message, cause);
    }

    /**
     * Retrieve fault details to pass to the XSL transformation as XSL parameters
     * 
     * @return The XSL parameters
     */
    public abstract Map<QName, String> getXslParameters();
}
