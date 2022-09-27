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

import java.util.Properties;

import org.flowable.common.engine.api.FlowableException;

public class PetalsUtil {

    private static Properties placeholders;

    private PetalsUtil() {
        // Utility class --> no constructor
    }

    public static void init(final Properties compPlaceholders) {
        placeholders = compPlaceholders;
    }

    public static void clean() {
        placeholders = null;
    }

    public static String getPlaceholder(final Object placeholderStr) {
        if (placeholderStr instanceof String) {
            return placeholders.getProperty((String) placeholderStr);
        } else {
            throw new FlowableException(
                    "The custom function 'petals:getPlaceholder('my-placeholder')' requires one argument as String.");
        }
    }

    public static String getPlaceholderWithDefault(final Object placeholderStr, final Object defaultValueStr) {
        if ((placeholderStr instanceof String) && (defaultValueStr instanceof String)) {
            return placeholders.getProperty((String) placeholderStr, (String) defaultValueStr);
        } else {
            throw new FlowableException(
                    "The custom function 'petals:getPlaceholder('my-placeholder', 'default value'))' requires two arguments as String.");
        }
    }
}
