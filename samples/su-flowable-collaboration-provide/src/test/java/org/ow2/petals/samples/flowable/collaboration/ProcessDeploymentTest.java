/**
 * Copyright (c) 2018-2026 Linagora
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
package org.ow2.petals.samples.flowable.collaboration;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.ow2.petals.flowable.utils.test.Await.waitEndOfProcessInstance;

import java.io.IOException;
import java.net.URL;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.logging.LogManager;

import javax.xml.namespace.QName;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.ow2.petals.flowable.junit.extensions.PetalsFlowableTest;
import org.ow2.petals.samples.flowable.collaboration.mock.CollaborationSvcMock;

import jakarta.xml.ws.Endpoint;

@PetalsFlowableTest
public class ProcessDeploymentTest {

    // -------------------------
    // Prepare service mocks
    // -------------------------
    private CollaborationSvcMock collaborationSvcMock;

    private Endpoint collaborationSvcEdp;

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

        final String urlCollaboration = "http://localhost:12345/collaboration";
        this.collaborationSvcMock = new CollaborationSvcMock(processEngine);
        this.collaborationSvcEdp = Endpoint.publish(urlCollaboration, this.collaborationSvcMock);
        endpoints.put(
                new QName("http://petals.ow2.org/samples/se-flowable/collaboration/services", "autogenerate"),
                new URL(urlCollaboration));

        ((ProcessEngineConfigurationImpl) processEngine.getProcessEngineConfiguration())
                .setWsOverridenEndpointAddresses(endpoints);
    }

    @AfterEach
    public void destroy() {
        this.collaborationSvcEdp.stop();
    }
    
    @Test
    @Deployment(resources = { "jbi/collaboration.bpmn" })
    public void nominal(final ProcessEngine processEngine) throws InterruptedException {

        // Assertion about the deployment of processes
        assertNotNull(processEngine.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("masterProcess").singleResult());
        assertNotNull(processEngine.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("childProcess").singleResult());

        final ProcessInstance masterProcessInst = processEngine.getRuntimeService()
                .startProcessInstanceByKey("masterProcess");
        assertNotNull(masterProcessInst);

        waitEndOfProcessInstance(masterProcessInst.getProcessInstanceId(), processEngine.getHistoryService());
        
        final HistoricProcessInstance masterProcHistInst = processEngine.getHistoryService()
                .createHistoricProcessInstanceQuery().processInstanceId(masterProcessInst.getId())
                .includeProcessVariables().singleResult();
        assertNotNull(masterProcHistInst);
        assertNotNull(masterProcHistInst.getProcessVariables());
        final Object childProcInstIdObj = masterProcHistInst.getProcessVariables().get("childProcessId");
        assertNotNull(childProcInstIdObj);
        assertTrue(childProcInstIdObj instanceof String);
        assertFalse(((String) childProcInstIdObj).trim().isEmpty());
    }
}
