/**
 * Copyright (c) 2019-2022 Linagora
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
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import org.awaitility.Awaitility;
import org.flowable.engine.repository.ProcessDefinition;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.flowable.task.api.Task;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.ow2.petals.flowable.junit.PetalsFlowableRule;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class ProcessDeploymentTest {
    
    private final ObjectMapper mapper = new ObjectMapper();

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
    public final PetalsFlowableRule flowableRule = new PetalsFlowableRule();
    
    @Test
    @Deployment(resources = { "jbi/travelingProcess.bpmn20.xml" })
    public void nominal() throws InterruptedException {
        final ProcessDefinition processDefinition = this.flowableRule.getRepositoryService()
                .createProcessDefinitionQuery().processDefinitionKey("travelingProcess").singleResult();
        assertNotNull(processDefinition);

        final ObjectNode routesObj = this.mapper.createObjectNode();
        final ArrayNode trains = routesObj.putArray("trains");
        final ObjectNode train1 = trains.addObject();
        train1.put("startFrom", "Nice");
        train1.put("endTo", "Paris");
        train1.put("date", "2019-08-28");
        final ObjectNode train2 = trains.addObject();
        train2.put("startFrom", "Paris");
        train2.put("endTo", "Rennes");
        train2.put("date", "2019-08-28");
        final ArrayNode planes = routesObj.putArray("planes");
        final ObjectNode plane = planes.addObject();
        plane.put("startFrom", "Rennes");
        plane.put("endTo", "Nice");
        plane.put("date", "2019-08-29");
        final ArrayNode hotels = routesObj.putArray("hotels");
        final ObjectNode hotel = hotels.addObject();
        hotel.put("address", "Paris");
        hotel.put("startDate", "2019-08-28");
        hotel.put("endDate", "2019-08-29");

        final Map<String, Object> initVariables = new HashMap<String, Object>();
        initVariables.put("numberOfDays", 10);
        initVariables.put("startDate", new Date());
        initVariables.put("route", routesObj);
        initVariables.put("travelingMotivation", "COTECH 2019");
        final ProcessInstance processInstance = this.flowableRule.getRuntimeService().startProcessInstanceByKey(
                "travelingProcess", initVariables);
        assertNotNull(processInstance);

        // Wait user task assignment
        Awaitility.await().atMost(60, TimeUnit.SECONDS).until(new Callable<Boolean>() {

            @Override
            public Boolean call() throws Exception {
                final Task task = flowableRule.getTaskService().createTaskQuery()
                        .processInstanceId(processInstance.getId()).taskDefinitionKey("handleTravelRequest")
                        .taskCandidateOrAssigned("kermit").singleResult();
                return task != null;
            }
        });
        final Task task = this.flowableRule.getTaskService().createTaskQuery()
                .processInstanceId(processInstance.getId()).taskDefinitionKey("handleTravelRequest")
                .taskCandidateOrAssigned("kermit").singleResult();
        final Map<String, Object> userTaskVariables = new HashMap<String, Object>();
        userTaskVariables.put("travelingApproved", true);
        this.flowableRule.getTaskService().complete(task.getId(), userTaskVariables);

        this.flowableRule.waitEndOfProcessInstance(processInstance.getId());
    }
}
