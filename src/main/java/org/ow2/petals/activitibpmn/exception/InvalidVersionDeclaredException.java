/**
 * Copyright (c) 2014-2016 Linagora
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
package org.ow2.petals.activitibpmn.exception;


/**
 * The version of a process definition is invalid (ie, not a number) into the JBI descriptor
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class InvalidVersionDeclaredException extends ProcessDefinitionDeclarationException {

    private static final long serialVersionUID = 1957844369186419634L;

    public static final String MESSAGE_PATTERN = "The version of the process file '%s' is invalid: %s";

    /**
     * @param processFileName
     *            The value of the process file of the SU JBI descriptor
     * @param version
     *            The value of the version of the SU JBI descriptor
     */
    public InvalidVersionDeclaredException(final String processFileName, final String version) {
        super(String.format(MESSAGE_PATTERN, processFileName, version));
    }

}
