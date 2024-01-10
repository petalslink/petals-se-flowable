/**
 * Copyright (c) 2019-2024 Linagora
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

public class VariableUnsupportedTypeException extends VariableException {

    private static final long serialVersionUID = 7254660201742155935L;

    private static final String MESSAGE_PATTERN = "Unsupported type '%s' for the variable '%s'.";

    public VariableUnsupportedTypeException(final String variableName, final String unsupportedType) {
        super(variableName, String.format(MESSAGE_PATTERN, unsupportedType, variableName));
    }

}
