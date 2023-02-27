/**
 * Copyright (c) 2019-2023 Linagora
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.GregorianCalendar;

import javax.jbi.messaging.MessagingException;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import org.junit.Test;
import org.ow2.petals.components.flowable.generic._1.Variable;

/**
 * Unit test of utility class {@link Utils}
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class UtilsTest {

    private static final String STRING_VAR_VALUE = "string-var-value";

    private static final String LONG_VAR_VALUE = "1243";

    private static final String DOUBLE_VAR_VALUE = "1243.9635";

    @Test
    public void parseVariable_nominal() throws MessagingException, DatatypeConfigurationException {

        final Variable variable = new Variable();
        variable.setName("my-variable");

        // Variable as 'string' or default
        variable.setValue(STRING_VAR_VALUE);

        variable.setAs(null);
        assertEquals(STRING_VAR_VALUE, Utils.parseVariableValue(variable));

        variable.setAs("string");
        assertEquals(STRING_VAR_VALUE, Utils.parseVariableValue(variable));

        // Variable as 'long'
        variable.setAs("long");

        variable.setValue(LONG_VAR_VALUE);
        assertEquals(Long.parseLong(LONG_VAR_VALUE), Utils.parseVariableValue(variable));

        variable.setValue("invalid-value");
        try {
            Utils.parseVariableValue(variable);
            fail("MessagingException not thrown because of invalid long value !!");
        } catch (final MessagingException e) {
            assertTrue(e.getCause() instanceof NumberFormatException);
        }

        // Variable as 'double'
        variable.setAs("double");

        variable.setValue(DOUBLE_VAR_VALUE);
        assertEquals(Double.parseDouble(DOUBLE_VAR_VALUE), Utils.parseVariableValue(variable));

        variable.setValue("invalid-value");
        try {
            Utils.parseVariableValue(variable);
            fail("MessagingException not thrown because of invalid long value !!");
        } catch (final MessagingException e) {
            assertTrue(e.getCause() instanceof NumberFormatException);
        }

        // Variable as 'boolean'
        variable.setAs("boolean");

        variable.setValue(Boolean.TRUE.toString());
        assertEquals(Boolean.TRUE, Utils.parseVariableValue(variable));

        variable.setValue(Boolean.FALSE.toString());
        assertEquals(Boolean.FALSE, Utils.parseVariableValue(variable));

        // Variable as 'date'
        variable.setAs("date");
        final GregorianCalendar calendar = new GregorianCalendar();
        variable.setValue(DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar).toXMLFormat());
        assertEquals(calendar.getTime(), Utils.parseVariableValue(variable));

        // Unknown type
        variable.setAs("unknown-type");
        try {
            Utils.parseVariableValue(variable);
            fail("MessagingException not thrown because of unknown type !!");
        } catch (final MessagingException e) {
            // NOP: Expected exception
        }

    }

}
