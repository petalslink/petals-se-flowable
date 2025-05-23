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
package org.ow2.petals.flowable.incoming.operation.annotated.exception;

import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;

/**
 * An error occurs about BPMN annotations
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class InvalidAnnotationException extends PEtALSCDKException {

    private static final long serialVersionUID = -5070343353540572994L;

    public InvalidAnnotationException(final String message) {
        super(message);
    }

    public InvalidAnnotationException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
