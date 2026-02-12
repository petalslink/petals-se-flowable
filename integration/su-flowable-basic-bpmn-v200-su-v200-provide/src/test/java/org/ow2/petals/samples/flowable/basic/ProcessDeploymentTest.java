/**
 * Copyright (c) 2020-2026 Linagora
 *
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the New BSD License (3-clause license).
 *
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the New BSD License (3-clause license)
 * for more details.
 *
 * You should have received a copy of the New BSD License (3-clause license)
 * along with this program/library; If not, see http://directory.fsf.org/wiki/License:BSD_3Clause/
 * for the New BSD License (3-clause license).
 */
package org.ow2.petals.samples.flowable.basic;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.ow2.petals.flowable.utils.test.Assert.assertCurrentUserTask;
import static org.ow2.petals.flowable.utils.test.Await.waitEndOfProcessInstance;
import static org.ow2.petals.flowable.utils.test.Await.waitUserTaskAssignment;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.logging.LogManager;

import javax.xml.namespace.QName;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.flowable.task.api.Task;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.ow2.petals.flowable.junit.extensions.PetalsFlowableTest;
import org.ow2.petals.samples.flowable.basic.mock.LogServiceMock;

import jakarta.xml.ws.Endpoint;

@PetalsFlowableTest
public class ProcessDeploymentTest {

    // -------------------------
    // Prepare service mocks
    // -------------------------
    private final LogServiceMock logServiceMock = new LogServiceMock();

    private Endpoint logServiceEdp;

    static {
        final URL logConfig = ProcessDeploymentTest.class.getResource("/logging.properties");
        assertNotNull(logConfig, "Logging configuration file not found");

        try {
            LogManager.getLogManager().readConfiguration(logConfig.openStream());
        } catch (final SecurityException | IOException e) {
            throw new AssertionError(e);
        }
    }

    @BeforeEach
    public void setupEndpoints(final ProcessEngine processEngine) throws Exception {
        final ConcurrentMap<QName, URL> endpoints = new ConcurrentHashMap<>();

        final String urlCollaboration = "http://localhost:12345/logService";
        this.logServiceEdp = Endpoint.publish(urlCollaboration, this.logServiceMock);
        endpoints.put(new QName("http://petals.ow2.org/integration/tests/se-flowable/log/services/v2", "autogenerate"),
                new URL(urlCollaboration));

        ((ProcessEngineConfigurationImpl) processEngine.getProcessEngineConfiguration())
                .setWsOverridenEndpointAddresses(endpoints);
    }

    @AfterEach
    public void destroy() {
        this.logServiceEdp.stop();
    }

    @Test
    @Deployment(resources = { "jbi/basic.bpmn" })
    public void nominal(final ProcessEngine processEngine) throws InterruptedException {

        // Assertion about the deployment of processes
        assertNotNull(processEngine.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("basicProcess").singleResult());

        final Map<String, Object> variables = new HashMap<>();
        variables.put("handlerName", "my-handler");
        final ProcessInstance masterBasicInst = processEngine.getRuntimeService()
                .startProcessInstanceByKey("basicProcess", variables);
        assertNotNull(masterBasicInst);

        waitUserTaskAssignment(masterBasicInst.getProcessInstanceId(), "usertask1", "kermit",
                processEngine.getTaskService());
        final Task userTask1 = assertCurrentUserTask(masterBasicInst.getProcessInstanceId(), "usertask1", "kermit",
                processEngine.getTaskService());
        processEngine.getTaskService().complete(userTask1.getId());

        waitEndOfProcessInstance(masterBasicInst.getProcessInstanceId(), processEngine.getHistoryService());

        final HistoricProcessInstance masterBasicHistInst = processEngine.getHistoryService()
                .createHistoricProcessInstanceQuery().processInstanceId(masterBasicInst.getId())
                .includeProcessVariables().singleResult();
        assertNotNull(masterBasicHistInst);
    }
}
