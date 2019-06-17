/**
 * Copyright (c) 2017-2019 Linagora
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
package org.ow2.petals.flowable;

import javax.jbi.messaging.ExchangeStatus;

import org.flowable.engine.repository.ProcessDefinition;
import org.flowable.engine.runtime.ProcessInstance;
import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToProviderMessage;
import org.ow2.petals.se_flowable.unit_test.start_stop.Start;
import org.ow2.petals.se_flowable.unit_test.start_stop.StartResponse;

/**
 * Unit tests about request processing of BPMN services against lifecycle transition start/stop of SE Flowable
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class StartStopTest extends StartStopTestEnvironment {

    /**
     * <p>
     * Check the service invocation against lifecycle transition start/stop of SE Flowable
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the async executor is not started if SE Flowable not started,</li>
     * <li>the async executor is started when we start the SE FLowable,</li>
     * <li>the async executor is stopped when we stop the SE Flowable.</li>
     * </ul>
     */
    @Test
    public void startStop() throws Exception {

        /*---------------------------------------------------------------------
         * The Async executor is not started if SE Flowable just initialized
         *--------------------------------------------------------------------*/
        assertTrue(COMPONENT_UNDER_TEST.isInstalled());
        assertFalse(COMPONENT_UNDER_TEST.isStarted());

        // We start a process instance through a Flowable client because the SE Flowable does not process incoming
        // request because it is not started
        final ProcessDefinition procDef = this.flowableClient.getRepositoryService().createProcessDefinitionQuery()
                .singleResult();
        assertNotNull(procDef);
        final ProcessInstance procInst1 = this.flowableClient.getRuntimeService()
                .startProcessInstanceById(procDef.getId());
        assertNotNull(procInst1);

        // Wait a potential async executor run: timer duration + 5s. Note: the Async executor is set to be run each 1s
        Thread.sleep(TIMER_DURATION_MS + 5000);

        this.assertProcessInstancePending(procInst1.getId(), BPMN_PROCESS_DEFINITION_KEY);

        /*-----------------------------------------------------------------------------------------------------------------------
         * When starting the SE Flowable, the async executor is started and the process instance will be automatically completed
         *----------------------------------------------------------------------------------------------------------------------*/
        COMPONENT_UNDER_TEST.start();
        assertTrue(COMPONENT_UNDER_TEST.isStarted());

        this.waitEndOfProcessInstance(procInst1.getId());

        /*----------------------------------------------------------------------------------------------
         * Start a new process instance for which the SE FLowable will be stopped during its execution
         *----------------------------------------------------------------------------------------------*/
        final StringBuilder procInst2 = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, START_STOP_SU,
                OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

        COMPONENT_UNDER_TEST.pushRequestToProvider(request);
        final ResponseMessage response = COMPONENT_UNDER_TEST.pollResponseFromProvider();
        assertNotNull("No XML payload in response", response.getPayload());
        final Object responseObj = UNMARSHALLER.unmarshal(response.getPayload());
        assertTrue(responseObj instanceof StartResponse);
        final StartResponse responseBean = (StartResponse) responseObj;
        assertNotNull(responseBean.getCaseFileNumber());
        procInst2.append(responseBean.getCaseFileNumber());
        COMPONENT_UNDER_TEST.pushStatusToProvider(new StatusToProviderMessage(response, ExchangeStatus.DONE));

        // Wait the processing of the status message
        Thread.sleep(5000);

        COMPONENT_UNDER_TEST.stop();
        assertTrue(COMPONENT_UNDER_TEST.isInstalled());
        assertFalse(COMPONENT_UNDER_TEST.isStarted());

        // Wait a potential async executor run: timer duration + 5s. Note: the Async executor is set to be run each 1s
        Thread.sleep(TIMER_DURATION_MS);

        /*-----------------------------------------------------------------------------------------
         * When re-starting the SE Flowable, the process instance will be automatically completed
         *----------------------------------------------------------------------------------------*/
        COMPONENT_UNDER_TEST.start();
        assertTrue(COMPONENT_UNDER_TEST.isStarted());

        this.waitEndOfProcessInstance(procInst2.toString());

    }
}
