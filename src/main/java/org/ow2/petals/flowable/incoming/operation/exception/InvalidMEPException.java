/**
 * Copyright (c) 2017-2018 Linagora
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

import javax.xml.namespace.QName;

/**
 * The current MEP of the exchange has no sens for the current Flowable operation.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class InvalidMEPException extends OperationProcessingException {

    private static final long serialVersionUID = 5613465110782155196L;

    private static final String MESSAGE_PATTERN = "The current MEP '%s' of the exchange has no sens for the current Flowable operation '%s'.";

    public InvalidMEPException(final QName wsdlOperation, final String currentMEP, final String flowableOperation) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, currentMEP, flowableOperation));
    }
}
