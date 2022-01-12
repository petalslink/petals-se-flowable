/**
 * Copyright (c) 2020-2022 Linagora
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

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.xml.namespace.QName;
import javax.xml.ws.Endpoint;

import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.flowable.task.api.Task;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.ow2.petals.flowable.junit.PetalsFlowableRule;
import org.ow2.petals.samples.flowable.basic.mock.LogServiceMock;

public class ProcessDeploymentTest {

    @Rule
    public final PetalsFlowableRule flowableRule = new PetalsFlowableRule();

    // -------------------------
    // Prepare service mocks
    // -------------------------
    private final LogServiceMock logServiceMock = new LogServiceMock();

    private Endpoint logServiceEdp;

    @BeforeClass
    public static void initLog() throws URISyntaxException {
        final URL logCfg = Thread.currentThread().getContextClassLoader().getResource("logging.properties");
        assertNotNull("Log conf file not found", logCfg);
        System.setProperty("java.util.logging.config.file", new File(logCfg.toURI()).getAbsolutePath());
    }

    @Before
    public void setupEndpoints() throws Exception {
        final ConcurrentMap<QName, URL> endpoints = new ConcurrentHashMap<>();

        final String urlCollaboration = "http://localhost:12345/logService";
        this.logServiceEdp = Endpoint.publish(urlCollaboration, this.logServiceMock);
        endpoints.put(new QName("http://petals.ow2.org/integration/tests/se-flowable/log/services/v2", "autogenerate"),
                new URL(urlCollaboration));

        ((ProcessEngineConfigurationImpl) this.flowableRule.getProcessEngine().getProcessEngineConfiguration())
                .setWsOverridenEndpointAddresses(endpoints);
    }

    @After
    public void destroy() {
        this.logServiceEdp.stop();
    }

    @AfterClass
    public static void cleanLog() {
        System.clearProperty("java.util.logging.config.file");
    }

    @Test
    @Deployment(resources = { "jbi/basic.bpmn" })
    public void nominal() throws InterruptedException {

        // Assertion about the deployment of processes
        assertNotNull(this.flowableRule.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("basicProcess").singleResult());

        final Map<String, Object> variables = new HashMap<>();
        variables.put("handlerName", "my-handler");
        final ProcessInstance masterBasicInst = this.flowableRule.getRuntimeService()
                .startProcessInstanceByKey("basicProcess", variables);
        assertNotNull(masterBasicInst);

        this.flowableRule.waitUserTaskAssignment(masterBasicInst.getProcessInstanceId(), "usertask1", "kermit");
        final Task userTask1 = this.flowableRule.assertCurrentUserTask(masterBasicInst.getProcessInstanceId(),
                "usertask1", "kermit");
        this.flowableRule.getTaskService().complete(userTask1.getId());

        this.flowableRule.waitEndOfProcessInstance(masterBasicInst.getProcessInstanceId());

        final HistoricProcessInstance masterBasicHistInst = this.flowableRule.getHistoryService()
                .createHistoricProcessInstanceQuery().processInstanceId(masterBasicInst.getId())
                .includeProcessVariables().singleResult();
        assertNotNull(masterBasicHistInst);
    }
}
