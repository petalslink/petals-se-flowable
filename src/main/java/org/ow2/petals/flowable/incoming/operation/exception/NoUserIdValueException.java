/**
 * Copyright (c) 2015-2019 Linagora
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

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;


/**
 * The value of the user identifier is required in the incoming request.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class NoUserIdValueException extends OperationProcessingFault {

    private static final long serialVersionUID = -5968252264228963812L;

    private static final String MESSAGE = "User identifier is missing or empty in the incoming request !";

    public NoUserIdValueException(final QName wsdlOperation) {
        super(wsdlOperation, MESSAGE);
    }

    @Override
    public Map<QName, String> getXslParameters() {
        final Map<QName, String> xslParameters = new HashMap<QName, String>();
        return xslParameters;
    }

}
