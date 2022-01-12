/**
 * Copyright (c) 2018-2022 Linagora
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
 * Process definition not found.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ProcessDefinitionNotFoundException extends PetalsException {

    private static final long serialVersionUID = -5916104834433537994L;

    private static final String PATTERN = "Process definition not found: %s version %d";

    private final String procDefKey;
    
    private final int procDefVer;

    public ProcessDefinitionNotFoundException(final String procDefKey, final int procDefVer) {
        super(String.format(PATTERN, procDefKey, procDefVer));
        this.procDefKey = procDefKey;
        this.procDefVer = procDefVer;
    }

    public String getProcDefKey() {
        return this.procDefKey;
    }

    public int getProcDefVer() {
        return this.procDefVer;
    }

}
