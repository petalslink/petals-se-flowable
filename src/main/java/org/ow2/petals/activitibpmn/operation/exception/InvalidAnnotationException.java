/**
 * Copyright (c) 2014 Linagora
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
package org.ow2.petals.activitibpmn.operation.exception;

import org.ow2.petals.activitibpmn.operation.ActivitiOperation;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;

/**
 * A WSDL annotation is invalid for the operation
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class InvalidAnnotationException extends PEtALSCDKException {

    private static final long serialVersionUID = 4807726306653521594L;

    private static final String MESSAGE_PATTERN = "A WSDL annotation is invalid for the operation '%s': %s";

    public InvalidAnnotationException(final ActivitiOperation operation, final String message) {
        super(String.format(MESSAGE_PATTERN, operation.getName(), message));
    }

    public InvalidAnnotationException(final ActivitiOperation operation, final String message, final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, operation.getName(), message), cause);
    }
}
