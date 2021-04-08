/**
 * Copyright (c) 2019-2021 Linagora
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

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.ow2.petals.flowable.incoming.variable.exception.VariableException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableValueRequiredException;
import org.w3c.dom.Document;

/**
 * Abstract variable implementation for type with value extracted as {@code string}.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class VariableStringBasedValueImpl extends VariableImpl {

    public VariableStringBasedValueImpl(final String name, final XPathExpression xpathExprValue, final String type,
            final boolean isRequired, final Logger logger) {
        super(name, xpathExprValue, type, isRequired, logger);
    }

    /**
     * Return the Flowable variable value in the right format according to its type
     * 
     * @param variableValueAsStr
     *            The Flowable variable value as {@link String} to convert in the right type
     * @throws VariableException
     *             Invalid value
     */
    protected abstract Object convert(final String variableValueAsStr) throws VariableException;

    @Override
    public Object extract(final Document incomingPayload) throws VariableException {
        try {
            final String variableValueAsStr;
            synchronized (this.xpathExprValue) {
                variableValueAsStr = this.xpathExprValue.evaluate(incomingPayload);
            }
            if (variableValueAsStr == null || variableValueAsStr.trim().isEmpty()) {
                if (this.isRequired) {
                    throw new VariableValueRequiredException(this.name);
                } else {
                    if (this.logger.isLoggable(Level.FINE)) {
                        this.logEmptyValue();
                    }
                    return null;
                }
            } else {
                if (this.logger.isLoggable(Level.FINE)) {
                    this.logIntermediateValue("xml extraction", variableValueAsStr);
                }

                final Object variableValue = this.convert(variableValueAsStr);

                if (this.logger.isLoggable(Level.FINE)) {
                    this.logFinalValue(variableValue.toString());
                }
                return variableValue;
            }
        } catch (final XPathExpressionException e) {
            throw new VariableException(this.name, e);
        }
    }

}
