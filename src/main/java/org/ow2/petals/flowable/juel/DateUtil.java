/**
 * Copyright (c) 2022 Linagora
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
package org.ow2.petals.flowable.juel;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.flowable.common.engine.api.FlowableException;

public class DateUtil {

    private DateUtil() {
        // Utility class --> no constructor
    }

    public static Date parse(final Object dateStr, final Object dateFormat) {
        if ((dateStr instanceof String) && (dateFormat instanceof String)) {

            final SimpleDateFormat fmt = new SimpleDateFormat((String) dateFormat);
            try {
                return fmt.parse((String) dateStr);
            } catch (final ParseException e) {
                throw new FlowableException(
                        String.format("Error parsing date '%s' against the pattern '%s'", dateStr, dateFormat), e);
            }
        } else {
            throw new FlowableException("The custom function 'date:parse' requires two arguments as String.");
        }
    }
}
