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
package org.ow2.petals.flowable.incoming.variable;

import java.util.logging.Logger;

import javax.xml.xpath.XPathExpression;

import org.ow2.petals.flowable.incoming.variable.exception.VariableException;
import org.w3c.dom.Document;

/**
 * Abstract variable implementation.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class VariableImpl {

    protected final String name;

    protected final XPathExpression xpathExprValue;

    protected String type;

    protected boolean isRequired;

    protected Logger logger;

    protected VariableImpl(final String name, final XPathExpression xpathExprValue, final String type,
            final boolean isRequired, final Logger logger) {
        super();
        this.name = name;
        this.xpathExprValue = xpathExprValue;
        this.type = type;
        this.isRequired = isRequired;
        this.logger = logger;
    }

    public String getName() {
        return this.name;
    }

    public String getType() {
        return this.type;
    }

    public boolean isRequired() {
        return this.isRequired;
    }

    /**
     * 
     * @param incomingPayload
     * @return The variable value, or {@code null} if empty and not required
     * @throws VariableException
     */
    public abstract Object extract(final Document incomingPayload) throws VariableException;

    protected void logEmptyValue() {
        this.logFinalValue("no value");
    }

    protected void logIntermediateValue(final String intermediateStep, final String variableValue) {
        this.logger.fine(String.format("intermediate (%s) variable value for '%s' (type: %s): %s", intermediateStep,
                this.name, this.type,
                variableValue));
    }

    protected void logFinalValue(final String variableValue) {
        this.logger.fine(String.format("variable value for '%s' (type: %s): %s", this.name, this.type, variableValue));
    }

}
