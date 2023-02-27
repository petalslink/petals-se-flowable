/**
 * Copyright (c) 2018-2023 Linagora
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
package org.ow2.petals.se.flowable.clientserver.api.admin.exception;

import org.ow2.petals.basisapi.exception.PetalsException;

/**
 * Process instance not found.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ProcessInstanceNotFoundException extends PetalsException {

    private static final long serialVersionUID = 7885265722189990410L;

    private static final String PATTERN = "Process instance not found: %s";

    private final String procInstNotFound;

    public ProcessInstanceNotFoundException(final String procInstNotFound) {
        super(String.format(PATTERN, procInstNotFound));
        this.procInstNotFound = procInstNotFound;
    }

    public ProcessInstanceNotFoundException(final String procInstNotFound, final Throwable cause) {
        super(String.format(PATTERN, procInstNotFound), cause);
        this.procInstNotFound = procInstNotFound;
    }

    public String getProcInstNotFound() {
        return this.procInstNotFound;
    }

}
