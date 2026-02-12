/**
 * Copyright (c) 2015-2026 Linagora
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
package org.ow2.petals.samples.flowable;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.net.URL;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.LogManager;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.repository.ProcessDefinition;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.junit.jupiter.api.Test;
import org.ow2.petals.flowable.junit.extensions.PetalsFlowableTest;

@PetalsFlowableTest
public class ProcessDeploymentTest {

    static {
        final URL logConfig = ProcessDeploymentTest.class.getResource("/logging.properties");
        assertNotNull(logConfig, "Logging configuration file not found");

        try {
            LogManager.getLogManager().readConfiguration(logConfig.openStream());
        } catch (final SecurityException | IOException e) {
            throw new AssertionError(e);
        }
    }
    
    @Test
    @Deployment(resources = {"jbi/vacationRequest.bpmn20.xml"})
    public void theProcessIsDeployable(final ProcessEngine processEngine) {
        final ProcessDefinition processDefinition = processEngine.getRepositoryService()
                .createProcessDefinitionQuery().processDefinitionKey("vacationRequest").singleResult();
        assertNotNull(processDefinition);

        final Map<String, Object> variables = new HashMap<String, Object>();
        variables.put("numberOfDays", 10);
        variables.put("startDate", new Date());
        variables.put("vacationMotivation", "Vacations");
        final ProcessInstance processInstance = processEngine.getRuntimeService()
                .startProcessInstanceByKey(
                "vacationRequest", variables);
        assertNotNull(processInstance);
    }
}
