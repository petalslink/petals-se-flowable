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
 * A petals function that can be used in EL expressions to get a placeholder value with a default value if not defined.
 * 
 * @author Christophe DENEUX
 */
public class FlowablePetalsGetPlaceholderWithDefaultValueFunctionDelegate extends AbstractFlowableFunctionDelegate {

    @Override
    public String prefix() {
        return "petals";
    }

    @Override
    public String localName() {
        return "getPlaceholderWithDefault";
    }

    @Override
    public Class<?> functionClass() {
        return PetalsUtil.class;
    }

    @Override
    public Method functionMethod() {
        return getTwoObjectParameterMethod();
    }

}
