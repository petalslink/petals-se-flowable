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
package org.flowable.engine.impl.test;

import java.util.Arrays;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.ProcessEngineConfiguration;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.ow2.petals.flowable.juel.FlowableDateParseFunctionDelegate;

public class PetalsSEJunitTestHelper {

    public static ProcessEngine createProcessEngine(final String configurationResource) {
        ProcessEngine processEngine = TestHelper.processEngines.get(configurationResource);
        if (processEngine == null) {
            final ProcessEngineConfiguration processEngineCfg = ProcessEngineConfiguration
                    .createProcessEngineConfigurationFromResource(configurationResource);
            if (processEngineCfg instanceof ProcessEngineConfigurationImpl) {
                final ProcessEngineConfigurationImpl pecImpl = (ProcessEngineConfigurationImpl) processEngineCfg;
                pecImpl.setCustomFlowableFunctionDelegates(Arrays.asList(new FlowableDateParseFunctionDelegate()));
            }
            processEngine = processEngineCfg.buildProcessEngine();
            TestHelper.processEngines.put(configurationResource, processEngine);
        }
        return processEngine;
    }

}
