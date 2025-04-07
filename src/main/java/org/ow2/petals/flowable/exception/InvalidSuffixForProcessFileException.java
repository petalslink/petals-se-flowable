/**
 * Copyright (c) 2018-2025 Linagora
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

import org.flowable.engine.impl.bpmn.deployer.ResourceNameUtil;

/**
 * The declared process file has a suffix not supported by Flowable.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class InvalidSuffixForProcessFileException extends ProcessDefinitionDeclarationException {

    private static final long serialVersionUID = 2070407366278589128L;

    public static final String MESSAGE_PATTERN = "The process file '%s' has a suffix unsupoprted by Flowable. Supported suffixes are: '%s'.";

    /**
     * @param processFileName
     *            The value of the process file of the SU JBI descriptor
     */
    public InvalidSuffixForProcessFileException(final String processFileName) {
        super(String.format(MESSAGE_PATTERN, processFileName,
                String.join(",", ResourceNameUtil.BPMN_RESOURCE_SUFFIXES)));
    }

}
