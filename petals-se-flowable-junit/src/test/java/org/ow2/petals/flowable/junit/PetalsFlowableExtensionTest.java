/**
 * Copyright (c) 2018-2024 Linagora
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.ow2.petals.flowable.utils.test.Assert.assertCurrentIntermediateCatchMessageEvent;
import static org.ow2.petals.flowable.utils.test.Await.waitEndOfProcessInstance;
import static org.ow2.petals.flowable.utils.test.Await.waitIntermediateCatchMessageEvent;
import static org.ow2.petals.flowable.utils.test.Signal.signalIntermediateCatchMessageEvent;

import java.io.IOException;
import java.net.URL;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.logging.LogManager;

import org.awaitility.Awaitility;
import org.flowable.engine.ProcessEngine;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.flowable.task.api.Task;
import org.flowable.task.api.TaskQuery;
import org.junit.jupiter.api.Test;
import org.ow2.petals.component.framework.api.util.Placeholders;
import org.ow2.petals.flowable.junit.extensions.PetalsFlowableTest;
import org.ow2.petals.flowable.junit.tasks.DummyJavaDelegate;
import org.ow2.petals.flowable.utils.test.Await;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

@PetalsFlowableTest
public class PetalsFlowableExtensionTest {

    static {
        final URL logConfig = Thread.currentThread().getContextClassLoader().getResource("logging.properties");
        assertNotNull(logConfig, "Logging configuration file not found");

        try {
            LogManager.getLogManager().readConfiguration(logConfig.openStream());
        } catch (final SecurityException | IOException e) {
            throw new AssertionError(e);
        }
    }

    /**
     * Check {@link Await#waitIntermediateCatchMessageEvent(String, String, org.flowable.engine.RuntimeService)} and
     * {@link org.ow2.petals.flowable.utils.test.Assert#assertCurrentIntermediateCatchMessageEvent(String, String, org.flowable.engine.RuntimeService)}
     */
    @Test
    @Deployment(resources = { "intermediate-catch-event-message.bpmn" })
    public void intermediaiteCatchEventMessage(final ProcessEngine processEngine) throws InterruptedException {

        // Assertion about the deployment of processes
        assertNotNull(processEngine.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("intermediateMessageCatchEventProcess").singleResult());

        {
            // -------------------------------------------------------
            // 1/ Wait the intermediate catch message event step
            // 2/ Assertion
            // 3/ Signal, doing a new search query
            // --------------------------------------------------------
            final ProcessInstance processInst = processEngine.getRuntimeService()
                    .startProcessInstanceByKey("intermediateMessageCatchEventProcess");
            assertNotNull(processInst);

            waitIntermediateCatchMessageEvent(processInst.getId(), "IntermediateMessageCatchEventMsg",
                    processEngine.getRuntimeService());
            assertCurrentIntermediateCatchMessageEvent(processInst.getId(), "IntermediateMessageCatchEventMsg",
                    processEngine.getRuntimeService());
            signalIntermediateCatchMessageEvent(processInst.getId(), "IntermediateMessageCatchEventMsg",
                    processEngine.getRuntimeService());

            waitEndOfProcessInstance(processInst.getProcessInstanceId(), processEngine.getHistoryService());

            final HistoricProcessInstance procHistInst = processEngine.getHistoryService()
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
            final ProcessInstance processInst = processEngine.getRuntimeService()
                    .startProcessInstanceByKey("intermediateMessageCatchEventProcess");
            assertNotNull(processInst);

            waitIntermediateCatchMessageEvent(processInst.getId(), "IntermediateMessageCatchEventMsg",
                    processEngine.getRuntimeService());
            final Execution execution = assertCurrentIntermediateCatchMessageEvent(processInst.getId(),
                    "IntermediateMessageCatchEventMsg", processEngine.getRuntimeService());
            signalIntermediateCatchMessageEvent(execution, "IntermediateMessageCatchEventMsg",
                    processEngine.getRuntimeService());

            waitEndOfProcessInstance(processInst.getProcessInstanceId(), processEngine.getHistoryService());

            final HistoricProcessInstance procHistInst = processEngine.getHistoryService()
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
    public void dateParse(final ProcessEngine processEngine) throws InterruptedException, IOException {

        // Assertion about the deployment of processes
        assertNotNull(processEngine.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("dateParse").singleResult());

        final String jsonString = "{ \"id\" : \"XY2563\", \"date\" :\"2022-05-31T17:59:53\"}";
        final ObjectMapper mapper = new ObjectMapper();
        final JsonNode jsonObj = mapper.readTree(jsonString);

        {
            final Map<String, Object> variables = new HashMap<>();
            variables.put("data", jsonObj);
            final ProcessInstance processInst = processEngine.getRuntimeService().startProcessInstanceByKey("dateParse",
                    variables);
            assertNotNull(processInst);

            waitEndOfProcessInstance(processInst.getProcessInstanceId(), processEngine.getHistoryService());

            final HistoricProcessInstance procHistInst = processEngine.getHistoryService()
                    .createHistoricProcessInstanceQuery().processInstanceId(processInst.getId()).finished()
                    .singleResult();
            assertNotNull(procHistInst);
        }
    }

    @Test
    @Deployment(resources = { "getPlaceholderWithDefault.bpmn" })
    public void getPlaceholderWithDefault(final ProcessEngine processEngine, final Placeholders placeholders)
            throws InterruptedException {

        // Assertion about the deployment of processes
        assertNotNull(processEngine.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("getPlaceholderWithDefault").singleResult());

        {
            final Map<String, Object> variables = new HashMap<>();
            final ProcessInstance processInst = processEngine.getRuntimeService()
                    .startProcessInstanceByKey("getPlaceholderWithDefault", variables);
            assertNotNull(processInst);

            Awaitility.await().atMost(Duration.ofMinutes(1)).until(new Callable<Boolean>() {
                @Override
                public Boolean call() throws Exception {
                    final TaskQuery query = processEngine.getTaskService().createTaskQuery()
                            .processInstanceId(processInst.getId()).taskDefinitionKey("usertask1");
                    return query.singleResult() != null;
                }
            });
            final Task userTask = processEngine.getTaskService().createTaskQuery()
                    .processInstanceId(processInst.getId()).taskDefinitionKey("usertask1").singleResult();

            assertEquals(DummyJavaDelegate.result1, "my-placeholder-default-value");

            placeholders.setPlaceholder("my-placeholder", "my-placeholder-value");
            processEngine.getTaskService().complete(userTask.getId());

            waitEndOfProcessInstance(processInst.getProcessInstanceId(), processEngine.getHistoryService());

            final HistoricProcessInstance procHistInst = processEngine.getHistoryService()
                    .createHistoricProcessInstanceQuery().processInstanceId(processInst.getId()).finished()
                    .singleResult();
            assertNotNull(procHistInst);

            assertEquals("my-placeholder-value", DummyJavaDelegate.result1);
        }
    }

    @Test
    @Deployment(resources = { "getPlaceholder.bpmn" })
    public void getPlaceholderTest(final ProcessEngine processEngine, final Placeholders placeholders)
            throws InterruptedException {

        placeholders.setPlaceholder("my-placeholder", "my-placeholder-value");
        // Assertion about the deployment of processes
        assertNotNull(processEngine.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("getPlaceholder").singleResult());

        {
            final Map<String, Object> variables = new HashMap<>();
            final ProcessInstance processInst = processEngine.getRuntimeService()
                    .startProcessInstanceByKey("getPlaceholder", variables);
            assertNotNull(processInst);

            waitEndOfProcessInstance(processInst.getProcessInstanceId(), processEngine.getHistoryService());

            final HistoricProcessInstance procHistInst = processEngine.getHistoryService()
                    .createHistoricProcessInstanceQuery().processInstanceId(processInst.getId()).finished()
                    .singleResult();
            assertNotNull(procHistInst);

            assertEquals("my-placeholder-value", DummyJavaDelegate.result1);
        }
    }
}
