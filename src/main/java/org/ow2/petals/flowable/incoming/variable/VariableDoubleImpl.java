/**
 * Copyright (c) 2019-2022 Linagora
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
package org.ow2.petals.flowable.incoming.variable;

import java.util.logging.Logger;

import javax.xml.xpath.XPathExpression;

import org.ow2.petals.flowable.incoming.variable.exception.VariableException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableFormatDoubleException;

/**
 * Variable implementation for type {@code double}.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class VariableDoubleImpl extends VariableStringBasedValueImpl {

    public VariableDoubleImpl(final String name, final XPathExpression xpathExprValue, final String type,
            final boolean isRequired, final Logger logger) {
        super(name, xpathExprValue, type, isRequired, logger);
    }

    @Override
    protected Object convert(final String variableValueAsStr) throws VariableException {
        try {
            return Double.valueOf(variableValueAsStr);
        } catch (final NumberFormatException e) {
            throw new VariableFormatDoubleException(this.name, variableValueAsStr);
        }
    }

}
