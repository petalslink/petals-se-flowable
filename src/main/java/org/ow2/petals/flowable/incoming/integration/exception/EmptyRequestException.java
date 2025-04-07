/**
 * Copyright (c) 2015-2025 Linagora
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
package org.ow2.petals.flowable.incoming.integration.exception;

import javax.xml.namespace.QName;

/**
 * The incoming request received by the integration service is empty.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class EmptyRequestException extends InvalidRequestException {

    private static final long serialVersionUID = 86643546833322342L;

    private static final String MESSAGE_PATTERN = "The incoming request received by the integration operation '%s' is empty";

    public EmptyRequestException(final QName operationName) {
        super(String.format(MESSAGE_PATTERN, operationName.toString()));
    }

}
