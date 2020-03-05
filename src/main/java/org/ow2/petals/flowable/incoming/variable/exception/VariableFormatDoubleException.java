/**
 * Copyright (c) 2019-2020 Linagora
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
package org.ow2.petals.flowable.incoming.variable.exception;

public class VariableFormatDoubleException extends VariableException {

    private static final long serialVersionUID = -3723927285435335512L;

    private static final String MESSAGE_PATTERN = "The value of the variable '%s' must be a double ! Current value is: '%s'.";

    public VariableFormatDoubleException(final String variableName, final String variableValue) {
        super(variableName, String.format(MESSAGE_PATTERN, variableName, variableValue));
    }

}
