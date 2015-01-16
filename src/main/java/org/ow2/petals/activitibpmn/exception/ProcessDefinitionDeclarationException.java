/**
 * Copyright (c) 2014-2015 Linagora
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
package org.ow2.petals.activitibpmn.exception;

import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;

/**
 * The declaration of a process definition in the JBI descriptor is invalid.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ProcessDefinitionDeclarationException extends PEtALSCDKException {

    private static final long serialVersionUID = 1520688799578933006L;

    private static final String MESSAGE_PATTERN = "Invalid JBI descriptor: %s";

    public ProcessDefinitionDeclarationException(final String message) {
        super(String.format(MESSAGE_PATTERN, message));
    }

    public ProcessDefinitionDeclarationException(final String message, final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, message), cause);
    }

    public ProcessDefinitionDeclarationException(final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, cause.getMessage()), cause);
    }
}
