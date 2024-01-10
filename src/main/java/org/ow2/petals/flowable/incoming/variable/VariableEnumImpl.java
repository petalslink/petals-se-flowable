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

import java.util.List;
import java.util.logging.Logger;

import javax.xml.xpath.XPathExpression;

import org.flowable.bpmn.model.FormValue;
import org.ow2.petals.flowable.incoming.variable.exception.VariableException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableFormatEnumException;

/**
 * Variable implementation for type {@code enum}.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class VariableEnumImpl extends VariableStringBasedValueImpl {

    private final List<FormValue> enumerations;

    public VariableEnumImpl(final String name, final XPathExpression xpathExprValue, final String type,
            final boolean isRequired, final List<FormValue> enumerations, final Logger logger) {
        super(name, xpathExprValue, type, isRequired, logger);
        this.enumerations = enumerations;
    }

    public List<FormValue> getEnumerations() {
        return this.enumerations;
    }

    @Override
    protected Object convert(final String variableValueAsStr) throws VariableException {
        boolean validValue = false;
        for (final FormValue enumeration : this.enumerations) {
            if (variableValueAsStr.equals(enumeration.getId())) {
                validValue = true;
            }
        }
        if (!validValue) {
            throw new VariableFormatEnumException(this.name, variableValueAsStr);
        } else {
            return variableValueAsStr;
        }
    }

}
