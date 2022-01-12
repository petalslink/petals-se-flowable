/**
 * Copyright (c) 2014-2022 Linagora
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
package org.ow2.petals.flowable.exception;


/**
 * The declared process file does not exist.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class UnexistingProcessFileException extends ProcessDefinitionDeclarationException {

    private static final long serialVersionUID = -3002738209273554173L;

    public static final String MESSAGE_PATTERN = "The process file '%s' does not exist.";

    /**
     * @param processFileName
     *            The value of the process file of the SU JBI descriptor
     * @param cause
     *            The real cause of the file inexistance
     */
    public UnexistingProcessFileException(final String processFileName, final Throwable cause) {
        super(String.format(MESSAGE_PATTERN, processFileName), cause);
    }

}
