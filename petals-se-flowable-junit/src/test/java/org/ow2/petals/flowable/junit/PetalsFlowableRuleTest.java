/**
 * Copyright (c) 2018-2022 Linagora
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
package org.ow2.petals.flowable.junit;

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class PetalsFlowableRuleTest {

    final ConcurrentMap<String, String> placeholders = new ConcurrentHashMap<>();

    @Rule
    public final PetalsFlowableRule flowableRule = new PetalsFlowableRule(placeholders);

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

    @Before
    public void setUp() throws Exception {
        placeholders.clear();
    }

    /**
     * Check {@link PetalsFlowableRule#waitIntermediateCatchMessageEvent(String, String)} and
     * {@link PetalsFlowableRule#assertCurrentIntermediateCatchMessageEvent(String, String)}
     */
    @Test
    @Deployment(resources = { "intermediate-catch-event-message.bpmn" })
    public void intermediaiteCatchEventMessage() throws InterruptedException {

        // Assertion about the deployment of processes
        assertNotNull(this.flowableRule.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("intermediateMessageCatchEventProcess").singleResult());

        {
            // -------------------------------------------------------
            // 1/ Wait the intermediate catch message event step
            // 2/ Assertion
            // 3/ Signal, doing a new search query
            // --------------------------------------------------------
            final ProcessInstance processInst = this.flowableRule.getRuntimeService()
                    .startProcessInstanceByKey("intermediateMessageCatchEventProcess");
            assertNotNull(processInst);

            this.flowableRule.waitIntermediateCatchMessageEvent(processInst.getId(),
                    "IntermediateMessageCatchEventMsg");
            this.flowableRule.assertCurrentIntermediateCatchMessageEvent(processInst.getId(),
                    "IntermediateMessageCatchEventMsg");
            this.flowableRule.signalIntermediateCatchMessageEvent(processInst.getId(),
                    "IntermediateMessageCatchEventMsg");

            this.flowableRule.waitEndOfProcessInstance(processInst.getProcessInstanceId());

            final HistoricProcessInstance procHistInst = this.flowableRule.getHistoryService()
                    .createHistoricProcessInstanceQuery().processInstanceId(processInst.getId()).finished()
                    .singleResult();
            assertNotNull(procHistInst);
        }
        {
            // -------------------------------------------------------
            // 1/ Wait the intermediate catch message event step
            // 2/ Assertion
            // 3/ Signal without doing a new search query
            // --------------------------------------------------------
            final ProcessInstance processInst = this.flowableRule.getRuntimeService()
                    .startProcessInstanceByKey("intermediateMessageCatchEventProcess");
            assertNotNull(processInst);

            this.flowableRule.waitIntermediateCatchMessageEvent(processInst.getId(),
                    "IntermediateMessageCatchEventMsg");
            final Execution execution = this.flowableRule.assertCurrentIntermediateCatchMessageEvent(
                    processInst.getId(), "IntermediateMessageCatchEventMsg");
            this.flowableRule.signalIntermediateCatchMessageEvent(execution, "IntermediateMessageCatchEventMsg");

            this.flowableRule.waitEndOfProcessInstance(processInst.getProcessInstanceId());

            final HistoricProcessInstance procHistInst = this.flowableRule.getHistoryService()
                    .createHistoricProcessInstanceQuery().processInstanceId(processInst.getId()).finished()
                    .singleResult();
            assertNotNull(procHistInst);
        }
    }

    /**
     * Check that the JUEL function date:parse is available into the SE Flowable JUnit framework
     */
    @Test
    @Deployment(resources = { "date-parse.bpmn" })
    public void dateParse() throws InterruptedException, IOException {

        // Assertion about the deployment of processes
        assertNotNull(this.flowableRule.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("dateParse").singleResult());

        final String jsonString = "{ \"id\" : \"XY2563\", \"date\" :\"2022-05-31T17:59:53\"}";
        final ObjectMapper mapper = new ObjectMapper();
        final JsonNode jsonObj = mapper.readTree(jsonString);

        {
            final Map<String, Object> variables = new HashMap<>();
            variables.put("data", jsonObj);
            final ProcessInstance processInst = this.flowableRule.getRuntimeService()
                    .startProcessInstanceByKey("dateParse", variables);
            assertNotNull(processInst);

            this.flowableRule.waitEndOfProcessInstance(processInst.getProcessInstanceId());

            final HistoricProcessInstance procHistInst = this.flowableRule.getHistoryService()
                    .createHistoricProcessInstanceQuery().processInstanceId(processInst.getId()).finished()
                    .singleResult();
            assertNotNull(procHistInst);
        }
    }

    @Test
    @Deployment(resources = {"getPlaceholderWithDefault-test.bpmn"})
    public void getPlaceholderWithDefaultTest() throws InterruptedException {
        placeholders.put("aPlaceholder","value from placeholder");
        // Assertion about the deployment of processes
        assertNotNull(this.flowableRule.getRepositoryService().createProcessDefinitionQuery()
                                       .processDefinitionKey("getPlaceholderWithDefault").singleResult());

        {
            final Map<String, Object> variables = new HashMap<>();
            final ProcessInstance processInst = this.flowableRule.getRuntimeService()
                                                                 .startProcessInstanceByKey("getPlaceholderWithDefault", variables);
            assertNotNull(processInst);

            this.flowableRule.waitEndOfProcessInstance(processInst.getProcessInstanceId());

            final HistoricProcessInstance procHistInst = this.flowableRule.getHistoryService()
                                                                          .createHistoricProcessInstanceQuery()
                                                                          .processInstanceId(processInst.getId())
                                                                          .finished()
                                                                          .singleResult();
            assertNotNull(procHistInst);
        }
    }

    @Test
    @Deployment(resources = {"getPlaceholder-test.bpmn"})
    public void getPlaceholderTest() throws InterruptedException {
        placeholders.put("aPlaceholder","value from placeholder");
        // Assertion about the deployment of processes
        assertNotNull(this.flowableRule.getRepositoryService().createProcessDefinitionQuery()
                                       .processDefinitionKey("getPlaceholder").singleResult());

        {
            final Map<String, Object> variables = new HashMap<>();
            final ProcessInstance processInst = this.flowableRule.getRuntimeService()
                                                                 .startProcessInstanceByKey("getPlaceholder", variables);
            assertNotNull(processInst);

            this.flowableRule.waitEndOfProcessInstance(processInst.getProcessInstanceId());

            final HistoricProcessInstance procHistInst = this.flowableRule.getHistoryService()
                                                                          .createHistoricProcessInstanceQuery()
                                                                          .processInstanceId(processInst.getId())
                                                                          .finished()
                                                                          .singleResult();
            assertNotNull(procHistInst);
        }
    }
}
