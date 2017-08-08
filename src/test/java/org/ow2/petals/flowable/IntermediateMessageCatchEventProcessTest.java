/**
 * Copyright (c) 2017 Linagora
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
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.flowable.incoming.operation.exception.MessageEventReceivedException;
import org.ow2.petals.flowable.incoming.operation.exception.UnexpectedMessageEventException;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.AlreadyUnlocked;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.NotLocked;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.Start;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.StartResponse;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.Unlock;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services, with a component configured with default values and a process
 * with call a BPMN element 'intermediate message catch event'
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IntermediateMessageCatchEventProcessTest extends IntermediateMessageCatchEventProcessTestEnvironment {

    /**
     * <p>
     * Check the message processing where: a valid request is sent to event receipt.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>if the event is received when not expected by the BPMN engine, an error is returned,</li>
     * <li>if the event is received when expected by the BPMN engine, no error occurs and the process execution
     * continues,</li>
     * <li>if the event is received when already recieved by the BPMN engine, an error is returned,</li>
     * <li>the process instance is automatically completed when all is correctly done.</li>
     * </ul>
     * </p>
     */
    @Test
    public void execute() throws Exception {

        // ------------------------------------------------------------
        // Create a new instance of the process definition:
        // - After creation, the process instance is waiting a user task completion
        // ------------------------------------------------------------

        // Create a new instance of the process definition
        final StringBuilder processInstance = new StringBuilder();
        {
            final Start start = new Start();
            start.setCustomer(BPMN_USER);

            // Send the 1st valid request for start event 'request
            final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_START,
                    AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(start));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request_1);
            final ResponseMessage response_1 = COMPONENT_UNDER_TEST.pollResponseFromProvider();
            final Source fault = response_1.getFault();
            assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
            assertNotNull("No XML payload in response", response_1.getPayload());
            final Object responseObj = UNMARSHALLER.unmarshal(response_1.getPayload());
            assertTrue(responseObj instanceof StartResponse);
            final StartResponse response = (StartResponse) responseObj;
            assertNotNull(response.getCaseFileNumber());
            processInstance.append(response.getCaseFileNumber());
        }

        assertProcessInstancePending(processInstance.toString(), BPMN_PROCESS_DEFINITION_KEY);
        waitUserTaskAssignment(processInstance.toString(), USER_TASK_1, BPMN_USER);

        // ----------------------------------------------------------------------------
        // Send the intermediate message event when not expected by the BPMN engine
        // - Expected status ERROR with MEP InOnly
        // - Expected fault with MEP RobustInOnly
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstance.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.ERROR, response.getStatus());
            assertTrue(response.getError() instanceof UnexpectedMessageEventException);
        }
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstance.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final ResponseMessage response = COMPONENT_UNDER_TEST.pollResponseFromProvider();
            assertEquals(ExchangeStatus.ACTIVE, response.getStatus());
            final Source fault = response.getFault();
            assertNotNull("No fault returns", fault);
            final Object responseObj = UNMARSHALLER.unmarshal(fault);
            assertTrue(responseObj instanceof NotLocked);
            final NotLocked responseBean = (NotLocked) responseObj;
            assertEquals(processInstance.toString(), responseBean.getInstanceId());
            assertEquals("myMessageName", responseBean.getEventName());
        }

        assertProcessInstancePending(processInstance.toString(), BPMN_PROCESS_DEFINITION_KEY);
        waitUserTaskAssignment(processInstance.toString(), USER_TASK_1, BPMN_USER);

        // Complete the user task
        this.flowableClient.completeUserTask(processInstance.toString(), USER_TASK_1, BPMN_USER);
        assertUserTaskEnded(processInstance.toString(), USER_TASK_1, BPMN_USER);

        // ----------------------------------------------------------------------------
        // Send the intermediate message event when expected by the BPMN engine
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstance.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.DONE, response.getStatus());
        }

        assertProcessInstancePending(processInstance.toString(), BPMN_PROCESS_DEFINITION_KEY);
        waitUserTaskAssignment(processInstance.toString(), USER_TASK_2, BPMN_USER);

        // ----------------------------------------------------------------------------
        // Send the intermediate message event when it was already processed by the BPMN engine
        // - Expected status ERROR with MEP InOnly
        // - Expected fault with MEP RobustInOnly
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstance.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.ERROR, response.getStatus());
            assertTrue(response.getError() instanceof MessageEventReceivedException);
        }
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstance.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final ResponseMessage response = COMPONENT_UNDER_TEST.pollResponseFromProvider();
            assertEquals(ExchangeStatus.ACTIVE, response.getStatus());
            final Source fault = response.getFault();
            assertNotNull("No fault returns", fault);
            final Object responseObj = UNMARSHALLER.unmarshal(fault);
            assertTrue(responseObj instanceof AlreadyUnlocked);
            final AlreadyUnlocked responseBean = (AlreadyUnlocked) responseObj;
            assertEquals(processInstance.toString(), responseBean.getInstanceId());
            assertEquals("myMessageName", responseBean.getEventName());
        }

        // Complete the 2nd user task
        this.flowableClient.completeUserTask(processInstance.toString(), USER_TASK_2, BPMN_USER);
        assertUserTaskEnded(processInstance.toString(), USER_TASK_2, BPMN_USER);

        // Wait the end of the process instance
        waitEndOfProcessInstance(processInstance.toString());

        // Assertions about state of process instance at Flowable Level
        assertProcessInstanceFinished(processInstance.toString());

        // Check MONIT traces
        // TODO: To do when MONIT traces will be defined

    }
}
