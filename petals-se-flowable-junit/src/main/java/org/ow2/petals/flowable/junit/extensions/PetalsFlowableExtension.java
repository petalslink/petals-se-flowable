/**
 * Copyright (c) 2017-2026 Linagora
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
package org.ow2.petals.flowable.junit.extensions;

import java.util.logging.Logger;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.impl.test.PetalsSEJunitTestHelper;
import org.flowable.engine.test.FlowableExtension;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.ow2.petals.component.framework.api.util.Placeholders;
import org.ow2.petals.flowable.juel.PetalsUtil;

public class PetalsFlowableExtension extends FlowableExtension {

    private static final Logger LOG = Logger.getLogger(PetalsFlowableExtension.class.getName());

    private final Placeholders placeholders;

    public PetalsFlowableExtension() {
        super();
        this.placeholders = new Placeholders();
    }

    protected ProcessEngine createProcessEngine(final ExtensionContext context) {
        // We configure custom JUEL functions ...
        PetalsUtil.init(this.placeholders);

        return PetalsSEJunitTestHelper.createProcessEngine(getConfigurationResource(context), LOG);
    }

    @Override
    public boolean supportsParameter(final ParameterContext parameterContext, final ExtensionContext context) {
        final Class<?> parameterType = parameterContext.getParameter().getType();
        return Placeholders.class.equals(parameterType) || super.supportsParameter(parameterContext, context);
    }

    @Override
    public Object resolveParameter(final ParameterContext parameterContext, final ExtensionContext context) {

        final Class<?> parameterType = parameterContext.getParameter().getType();
        if (parameterType.isAssignableFrom(Placeholders.class)) {
            return this.placeholders;
        }
        return super.resolveParameter(parameterContext, context);
    }

    @Override
    public void afterEach(final ExtensionContext context) throws Exception {
        this.placeholders.clear();
        super.afterEach(context);
    }
}
