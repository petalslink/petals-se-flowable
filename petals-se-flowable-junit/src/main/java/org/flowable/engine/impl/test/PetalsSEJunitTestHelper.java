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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.ProcessEngineConfiguration;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.engine.parse.BpmnParseHandler;
import org.ow2.petals.flowable.CallActivityForceAsyncParseHandler;
import org.ow2.petals.flowable.ServiceTaskForceAsyncParseHandler;
import org.ow2.petals.flowable.juel.FlowableDateParseFunctionDelegate;
import org.ow2.petals.flowable.juel.FlowablePetalsGetPlaceholderFunctionDelegate;
import org.ow2.petals.flowable.juel.FlowablePetalsGetPlaceholderWithDefaultValueFunctionDelegate;


public class PetalsSEJunitTestHelper {

    public static ProcessEngine createProcessEngine(final String configurationResource, final Logger log) {
        ProcessEngine processEngine = TestHelper.processEngines.get(configurationResource);
        if (processEngine == null) {
            final ProcessEngineConfiguration processEngineCfg = ProcessEngineConfiguration
                    .createProcessEngineConfigurationFromResource(configurationResource);
            if (processEngineCfg instanceof ProcessEngineConfigurationImpl) {

                // Add support of custom JUEL functions
                final ProcessEngineConfigurationImpl pecImpl = (ProcessEngineConfigurationImpl) processEngineCfg;
                pecImpl.setCustomFlowableFunctionDelegates(Arrays.asList(
                    new FlowableDateParseFunctionDelegate(),
                    new FlowablePetalsGetPlaceholderFunctionDelegate(),
                    new FlowablePetalsGetPlaceholderWithDefaultValueFunctionDelegate()
                ));

                // Add post BPMN parse handlers
                final List<BpmnParseHandler> postBpmnParseHandlers = new ArrayList<>();
                postBpmnParseHandlers.add(new ServiceTaskForceAsyncParseHandler(log));
                postBpmnParseHandlers.add(new CallActivityForceAsyncParseHandler(log));
                pecImpl.setPostBpmnParseHandlers(postBpmnParseHandlers);
            }
            processEngine = processEngineCfg.buildProcessEngine();
            TestHelper.processEngines.put(configurationResource, processEngine);
        }
        return processEngine;
    }
}
