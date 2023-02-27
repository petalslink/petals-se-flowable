/**
 * Copyright (c) 2015-2023 Linagora
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
 * The annotation defining a fault is required for each fault of the given WSDL binding operation.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class NoFaultMappingException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 8306403158723168376L;

    private static final String MESSAGE_PATTERN = "The annotation defining the fault is required for the WSDL fault '%s'";

    /**
     * Name of the WSDL fault into the WSDL operation for which an error occurs
     */
    private final String wsdlFault;

    public NoFaultMappingException(final QName wsdlOperation, final String wsdlFault) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, wsdlFault));
        this.wsdlFault = wsdlFault;
    }

    /**
     * @return The name of the WSDL fault into the WSDL operation for which an error occurs
     */
    public String getWsdlFault() {
        return this.wsdlFault;
    }

}
