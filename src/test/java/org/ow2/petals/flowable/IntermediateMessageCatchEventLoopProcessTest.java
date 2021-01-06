/**
 * Copyright (c) 2019-2021 Linagora
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

import java.util.HashMap;
import java.util.Map;

import javax.jbi.messaging.ExchangeStatus;

import org.flowable.engine.repository.ProcessDefinition;
import org.flowable.engine.runtime.ProcessInstance;
import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event_loop.Unlock;

/**
 * Unit tests about request processing of BPMN services, with a component configured with default values and a process
 * with call a BPMN element 'intermediate message catch event' used in a loop
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IntermediateMessageCatchEventLoopProcessTest
        extends IntermediateMessageCatchEventLoopProcessTestEnvironment {

    private static final String VARIABLE_1_VALUE = "variable-1-value";

    private static final String VARIABLE_2_VALUE = "variable-2-value";

    /**
     * <p>
     * Check the processing of intermediate catch event used for loop when no technical error occurs during iteration
     * </p>
     */
    @Test
    public void execute() throws Exception {

        final ProcessDefinition deployment = this.flowableClient.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey(BPMN_PROCESS_DEFINITION_KEY).singleResult();
        final Map<String, Object> variables = new HashMap<>();
        variables.put("ctrl", "no-error");
        final ProcessInstance procInst = this.flowableClient.getRuntimeService()
                .startProcessInstanceById(deployment.getId(), variables);
        this.waitIntermediateCatchMessageEvent(procInst.getId(), MESSAGE_EVENT_NAME);

        // ----------------------------------------------------------------------------
        // Send the 1st intermediate message event when expected by the BPMN engine
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(procInst.getId());
            unlockRequest.setVar1(VARIABLE_1_VALUE);
            unlockRequest.setVar2(VARIABLE_2_VALUE);

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.DONE, response.getStatus());
        }

        this.assertProcessInstancePending(procInst.getId(), BPMN_PROCESS_DEFINITION_KEY);
        this.waitIntermediateCatchMessageEvent(procInst.getId(), MESSAGE_EVENT_NAME);

        // ----------------------------------------------------------------------------
        // Send the 2nd intermediate message event when expected by the BPMN engine
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(procInst.getId());
            unlockRequest.setVar1(VARIABLE_1_VALUE);
            unlockRequest.setVar2(VARIABLE_2_VALUE);

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.DONE, response.getStatus());
        }

        // Wait the end of the process instance
        this.waitEndOfProcessInstance(procInst.getId());

        // Assertions about state of process instance at Flowable Level
        this.assertProcessInstanceFinished(procInst.getId());

    }

    /**
     * <p>
     * Check the processing of intermediate catch event used for loop when a technical error occurs during iteration.
     * </p>
     */
    @Test
    public void error() throws Exception {

        final ProcessDefinition deployment = this.flowableClient.getRepositoryService().createProcessDefinitionQuery()
                .processDefinitionKey(BPMN_PROCESS_DEFINITION_KEY).singleResult();
        final Map<String, Object> variables = new HashMap<>();
        variables.put("ctrl", "error");
        final ProcessInstance procInst = this.flowableClient.getRuntimeService()
                .startProcessInstanceById(deployment.getId(), variables);
        this.waitIntermediateCatchMessageEvent(procInst.getId(), MESSAGE_EVENT_NAME);

        // ----------------------------------------------------------------------------
        // Send the 1st intermediate message event when expected by the BPMN engine
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(procInst.getId());
            unlockRequest.setVar1(VARIABLE_1_VALUE);
            unlockRequest.setVar2(VARIABLE_2_VALUE);

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.DONE, response.getStatus());
        }

        this.assertProcessInstancePending(procInst.getId(), BPMN_PROCESS_DEFINITION_KEY);
        this.waitIntermediateCatchMessageEvent(procInst.getId(), MESSAGE_EVENT_NAME);

        // ----------------------------------------------------------------------------
        // Send the 2nd intermediate message event when expected by the BPMN engine
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(procInst.getId());
            unlockRequest.setVar1(VARIABLE_1_VALUE);
            unlockRequest.setVar2(VARIABLE_2_VALUE);

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.DONE, response.getStatus());
        }

        // The script task fails because of a technical error
        this.waitProcessInstanceAsDeadLetterJob(procInst.getId());
        this.assertProcessInstancePending(procInst.getId(), BPMN_PROCESS_DEFINITION_KEY);
        // No subscription on event
        assertEquals(0, this.flowableClient.getRuntimeService().createExecutionQuery()
                .processInstanceId(procInst.getId()).messageEventSubscriptionName(MESSAGE_EVENT_NAME).list().size());

    }
}
