/**
 * Copyright (c) 2022-2025 Linagora
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

import java.lang.reflect.Method;

import org.flowable.common.engine.impl.el.AbstractFlowableFunctionDelegate;

/**
 * A date function mapper that can be used in EL expressions to parse date.
 * 
 * @author Christophe DENEUX
 */
// TODO: This custom JUEL function should be provided directly by Flowable. Move it to Flowable when custom
// JUEL functions will be put in a common library for all engines. Today with v6.4.2, each Flowable engine
// has its own custom JUEL functions.
public class FlowableDateParseFunctionDelegate extends AbstractFlowableFunctionDelegate {

    @Override
    public String prefix() {
        return "date";
    }

    @Override
    public String localName() {
        return "parse";
    }

    @Override
    public Class<?> functionClass() {
        return DateUtil.class;
    }

    @Override
    public Method functionMethod() {
        return getTwoObjectParameterMethod();
    }

}
