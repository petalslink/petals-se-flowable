/**
 * Copyright (c) 2015 Linagora
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

import org.ow2.petals.activitibpmn.ActivitiSEConstants;

/**
 * A process definition must be declared into the JBI descriptor using two fields:
 * <ul>
 * <li>a file,</li>
 * <li>a version.</li>
 * </ul>
 * One of both is missing.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IncoherentProcessDefinitionDeclarationException extends ProcessDefinitionDeclarationException {

    private static final long serialVersionUID = 1208127925422620310L;

    public static final String MESSAGE_PATTERN = "One of both fields '" + ActivitiSEConstants.PROCESS_FILE
            + "' (%s) and '" + ActivitiSEConstants.VERSION + "' (%s) is missing.";

    /**
     * @param processFileName
     *            The value of the process file of the SU JBI descriptor
     * @param version
     *            The value of the version of the SU JBI descriptor
     */
    public IncoherentProcessDefinitionDeclarationException(final String processFileName, final String version) {
        super(String.format(MESSAGE_PATTERN, processFileName, version));
    }

}
