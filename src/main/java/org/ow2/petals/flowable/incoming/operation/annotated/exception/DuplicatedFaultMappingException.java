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
package org.ow2.petals.flowable.incoming.operation.annotated.exception;

import javax.xml.namespace.QName;

/**
 * The annotation defining a fault is duplicated for a WSDL fault of the given WSDL binding operation.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class DuplicatedFaultMappingException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 5832090505390062659L;

    private static final String MESSAGE_PATTERN = "The annotation defining the fault '%s' is duplicated";

    /**
     * Name of the WSDL fault into the WSDL operation for which an error occurs
     */
    private final String wsdlFault;

    public DuplicatedFaultMappingException(final QName wsdlOperation, final String wsdlFault) {
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
