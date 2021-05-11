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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.ow2.petals.flowable.incoming.variable.exception.VariableUnsupportedTypeException;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Abstract variable implementation.
 *
 * @author Christophe DENEUX - Linagora
 *
 */
public class VariableImplBuilder {

    private VariableImplBuilder() {
        // Utility class --> No constrcutor
    }

    /**
     * Builds a {@link VariableImpl} from its {@link VariableDefinition}.
     *
     * @param variable
     *            The variable definition. Not {@code null}.
     * @return The variable implementation
     * @throws VariableUnsupportedTypeException
     *             The variable definition defines an unsupported type.
     */
    public static final VariableImpl build(final VariableDefinition variable, final ObjectMapper jacksonObjectMapper,
            final Logger logger) throws VariableUnsupportedTypeException {
        if ("string".equals(variable.getType())) {
            return new VariableStringImpl(variable.getName(), variable.getXpathExprValue(), variable.getType(),
                    variable.isRequired(), logger);
        } else if ("long".equals(variable.getType())) {
            return new VariableLongImpl(variable.getName(), variable.getXpathExprValue(), variable.getType(),
                    variable.isRequired(), logger);
        } else if ("double".equals(variable.getType())) {
            return new VariableDoubleImpl(variable.getName(), variable.getXpathExprValue(), variable.getType(),
                    variable.isRequired(), logger);
        } else if ("enum".equals(variable.getType())) {
            return new VariableEnumImpl(variable.getName(), variable.getXpathExprValue(), variable.getType(),
                    variable.isRequired(), variable.getEnumerations(), logger);
        } else if ("date".equals(variable.getType())) {
            return new VariableDateImpl(variable.getName(), variable.getXpathExprValue(), variable.getType(),
                    variable.isRequired(), variable.getDatePattern(), logger);
        } else if ("boolean".equals(variable.getType())) {
            return new VariableBooleanImpl(variable.getName(), variable.getXpathExprValue(), variable.getType(),
                    variable.isRequired(), logger);
        } else if ("json".equals(variable.getType())) {
            return new VariableJsonImpl(variable.getName(), variable.getXpathExprValue(), variable.getType(),
                    variable.isRequired(), variable.getPreXmlTransformation(),
                    variable.isPreXmlTranformationResultJson(), variable.getVirtualRoot(), variable.getMultiplePi(),
                    variable.getNamespaceMappings(),
                    jacksonObjectMapper, logger);
        } else {
            throw new VariableUnsupportedTypeException(variable.getName(), variable.getType());
        }
    }

    /**
     * Builds a list of {@link VariableImpl} from a list of {@link VariableDefinition}.
     *
     * @param variables
     *            The variable definitions. Not {@code null}.
     * @return The variable implementations
     * @throws VariableUnsupportedTypeException
     *             A variable definition defines an unsupported type.
     */
    public static final Map<String, VariableImpl> build(final Collection<VariableDefinition> variables,
            final ObjectMapper jacksonObjectMapper, final Logger logger)
            throws VariableUnsupportedTypeException {
        final Map<String, VariableImpl> variableImpls = new HashMap<>();
        for (final VariableDefinition variableDefinition : variables) {
            variableImpls.put(variableDefinition.getName(), build(variableDefinition, jacksonObjectMapper, logger));
        }
        return variableImpls;
    }

}
