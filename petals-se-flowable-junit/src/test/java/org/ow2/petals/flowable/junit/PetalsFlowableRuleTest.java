/**
 * Copyright (c) 2018-2020 Linagora
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
import java.net.URISyntaxException;
import java.net.URL;

import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.Deployment;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

public class PetalsFlowableRuleTest {

    @Rule
    public final PetalsFlowableRule flowableRule = new PetalsFlowableRule();

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

}
