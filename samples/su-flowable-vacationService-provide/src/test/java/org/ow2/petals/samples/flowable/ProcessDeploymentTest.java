/**
 * Copyright (c) 2015-2017 Linagora
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

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.activiti.engine.repository.ProcessDefinition;
import org.activiti.engine.runtime.ProcessInstance;
import org.activiti.engine.test.ActivitiRule;
import org.activiti.engine.test.Deployment;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

public class ProcessDeploymentTest {
    
    @BeforeClass
    public static void initLog() throws URISyntaxException {
        final URL logCfg = Thread.currentThread().getContextClassLoader().getResource("logging.properties");
        assertNotNull("Log conf file not found", logCfg);
        System.setProperty("java.util.logging.config.file", new File(logCfg.toURI()).getAbsolutePath());
    }

    @AfterClass
    public static void cleanLog() {
        System.clearProperty("java.util.logging.config.file");
    }

    @Rule
    public final ActivitiRule flowableRule = new ActivitiRule();
    
    @Test
    @Deployment(resources = {"jbi/vacationRequest.bpmn20.xml"})
    public void theProcessIsDeployable() {
        final ProcessDefinition processDefinition = this.flowableRule.getRepositoryService()
                .createProcessDefinitionQuery().processDefinitionKey("vacationRequest").singleResult();
        assertNotNull(processDefinition);

        final Map<String, Object> variables = new HashMap<String, Object>();
        variables.put("numberOfDays", 10);
        variables.put("startDate", new Date());
        variables.put("vacationMotivation", "Vacations");
        final ProcessInstance processInstance = this.flowableRule.getRuntimeService().startProcessInstanceByKey(
                "vacationRequest", variables);
        assertNotNull(processInstance);
    }
}
