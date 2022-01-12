/**
 * Copyright (c) 2018-2022 Linagora
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
package org.ow2.petals.flowable.incoming.integration;

import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import org.ow2.petals.components.flowable.generic._1.Variable;
import org.ow2.petals.components.flowable.generic._1.Variables;

public class Utils {

    private Utils() {
        // Utility class --> No constructor
    }

    public static Variables buildVariables(final Map<String, Object> variables, final Logger logger) {
        final Variables resultVariables = new Variables();
        for (final Entry<String, Object> taskProcessVariable : variables.entrySet()) {
            final Variable resultVariable = new Variable();
            resultVariable.setName(taskProcessVariable.getKey());
            final Object variableValue = taskProcessVariable.getValue();
            if (variableValue instanceof Date) {
                final GregorianCalendar calendar = new GregorianCalendar();
                calendar.setTime((Date) variableValue);
                try {
                    resultVariable
                            .setValue(DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar).toXMLFormat());
                } catch (final DatatypeConfigurationException e) {
                    logger.log(Level.WARNING,
                            String.format(
                                    "Error converting date value of variable '%s' into XML. Variable copy skiped !",
                                    taskProcessVariable.getKey()),
                            e);
                }

            } else {
                resultVariable.setValue(variableValue != null ? variableValue.toString() : "");
            }

            resultVariables.getVariable().add(resultVariable);
        }
        return resultVariables;
    }

    /**
     * Parse a variable value given through a XML payload ({@link Variable}) into a Flowable variable value
     * 
     * @param variable
     *            The variable to parse
     * @return The Flowable variable value.
     * @throws MessagingException
     *             An error occurs parsing the variable value.
     */
    public static Object parseVariableValue(final Variable variable) throws MessagingException {

        try {
            if (variable.getAs() == null || "string".equals(variable.getAs())) {
                return variable.getValue();
            } else if ("long".equals(variable.getAs())) {
                return Long.parseLong(variable.getValue());
            } else if ("double".equals(variable.getAs())) {
                return Double.parseDouble(variable.getValue());
            } else if ("date".equals(variable.getAs())) {
                return DatatypeFactory.newInstance().newXMLGregorianCalendar(variable.getValue()).toGregorianCalendar()
                        .getTime();
            } else if ("boolean".equals(variable.getAs())) {
                return Boolean.parseBoolean(variable.getValue());
            } else {
                throw new MessagingException(String.format("Unsupported type ('%s')for variable '%s'", variable.getAs(),
                        variable.getName()));
            }
        } catch (final NumberFormatException | DatatypeConfigurationException e) {
            throw new MessagingException(
                    String.format("Invalid value ('%s') for variable '%s' according to its type '%s'",
                            variable.getValue(), variable.getName(), variable.getAs()),
                    e);
        }

    }

}
