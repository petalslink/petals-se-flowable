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
package org.ow2.petals.samples.flowable.collaboration;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.xml.namespace.QName;
import javax.xml.ws.Endpoint;

import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.ow2.petals.flowable.junit.PetalsFlowableRule;
import org.ow2.petals.samples.flowable.collaboration.mock.CollaborationSvcMock;

public class ProcessDeploymentTest {

    @Rule
    public final PetalsFlowableRule flowableRule = new PetalsFlowableRule();

    // -------------------------
    // Prepare service mocks
    // -------------------------
    private final CollaborationSvcMock collaborationSvcMock = new CollaborationSvcMock(this.flowableRule);

    private Endpoint collaborationSvcEdp;
    
    @BeforeClass
    public static void initLog() throws URISyntaxException {
        final URL logCfg = Thread.currentThread().getContextClassLoader().getResource("logging.properties");
        assertNotNull("Log conf file not found", logCfg);
        System.setProperty("java.util.logging.config.file", new File(logCfg.toURI()).getAbsolutePath());
    }

    @Before
    public void setupEndpoints() throws Exception {
        final ConcurrentMap<QName, URL> endpoints = new ConcurrentHashMap<>();

        final String urlCollaboration = "http://localhost:12345/collaboration";
        this.collaborationSvcEdp = Endpoint.publish(urlCollaboration, this.collaborationSvcMock);
        endpoints.put(
                new QName("http://petals.ow2.org/samples/se-flowable/collaboration/services", "autogenerate"),
                new URL(urlCollaboration));

        ((ProcessEngineConfigurationImpl) this.flowableRule.getProcessEngine().getProcessEngineConfiguration())
                .setWsOverridenEndpointAddresses(endpoints);
    }

    @After
    public void destroy() {
        this.collaborationSvcEdp.stop();
    }

    @AfterClass
    public static void cleanLog() {
        System.clearProperty("java.util.logging.config.file");
    }
    
    @Test
    @Deployment(resources = { "jbi/collaboration.bpmn" })
    public void nominal() throws InterruptedException {

        // Assertion about the deployment of processes
        assertNotNull(this.flowableRule.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("masterProcess").singleResult());
        assertNotNull(this.flowableRule.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey("childProcess").singleResult());

        final ProcessInstance masterProcessInst = this.flowableRule.getRuntimeService()
                .startProcessInstanceByKey("masterProcess");
        assertNotNull(masterProcessInst);

        this.flowableRule.waitEndOfProcessInstance(masterProcessInst.getProcessInstanceId());
        
        final HistoricProcessInstance masterProcHistInst = this.flowableRule.getHistoryService()
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
