/**
 * Copyright (c) 2019-2026 Linagora
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Date;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.transform.Source;

import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.runtime.ProcessInstance;
import org.junit.jupiter.api.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToConsumerMessage;
import org.ow2.petals.se_flowable.unit_test.structured_variable.AckResponse;
import org.ow2.petals.se_flowable.unit_test.structured_variable.Approve;
import org.ow2.petals.se_flowable.unit_test.structured_variable.Start;
import org.ow2.petals.se_flowable.unit_test.structured_variable.StartResponse;
import org.ow2.petals.se_flowable.unit_test.structured_variable.TData;
import org.ow2.petals.se_flowable.unit_test.structured_variable.Unlock;
import org.ow2.petals.se_flowable.unit_test.structured_variable.alarm.Create;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * Unit tests about request processing of BPMN services, with a component configured with default values and a process
 * with call a BPMN element 'intermediate message catch event'
 * 
 * @author Christophe DENEUX - Linagora
 */
public class StructuredVariableProcessTest extends StructuredVariableTestEnvironment {

    private static final String VARIABLE_1_VALUE = "variable-1-value";

    private static final String VARIABLE_2_VALUE = "variable-2-value";

    /**
     * <p>
     * Check the message processing where: a valid request with a structured variable is sent.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the process instance is automatically completed when all is correctly done.</li>
     * </ul>
     */
    @Test
    public void execute() throws Exception {

        // Create a new instance of the process definition
        final StringBuilder processInstanceId = new StringBuilder();
        final Date vacationDate = new Date();
        {
            final Start start = new Start();
            start.setCustomer(BPMN_USER);
            final TData data = new TData();
            start.setData(data);
            data.setNumberOfDays(10);
            data.setStartDate(vacationDate);
            final String expectedVacationMotivation = "vacation motivation";
            data.setVacationMotivation(expectedVacationMotivation);

            // Send the 1st valid request for start event 'request
            final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    STRUCTURED_VARIABLE_SU, OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), start,
                    MARSHALLER);
            COMPONENT_UNDER_TEST.pushRequestToProvider(request_1);
            final ResponseMessage response_1 = COMPONENT_UNDER_TEST.pollResponseFromProvider();
            final Source fault = response_1.getFault();
            assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
            assertNotNull(response_1.getPayload(), "No XML payload in response");
            final Object responseObj = UNMARSHALLER.unmarshal(response_1.getPayload());
            assertInstanceOf(StartResponse.class, responseObj);
            final StartResponse response = (StartResponse) responseObj;
            assertNotNull(response.getCaseFileNumber());
            processInstanceId.append(response.getCaseFileNumber());

            // Assert that complementary variable defined by script task is correctly set
            final ProcessInstance procInst = FLOWABLE_CLIENT.getRuntimeService().createProcessInstanceQuery()
                    .processInstanceId(processInstanceId.toString()).includeProcessVariables().singleResult();
            assertNotNull(procInst);
            assertEquals(expectedVacationMotivation, procInst.getProcessVariables().get("vacationMotivation"));
        }

        this.assertProcessInstancePending(processInstanceId.toString(), BPMN_PROCESS_DEFINITION_KEY);
        this.waitUserTaskAssignment(processInstanceId.toString(), USER_TASK, BPMN_USER);

        // --------------------------------------------------------------
        // Complete the user task
        // --------------------------------------------------------------
        {
            final Approve approveRequest = new Approve();
            approveRequest.setCaseFileNumber(processInstanceId.toString());
            approveRequest.setApprover(BPMN_USER);
            final String expectedManagerMotivation = "Too long!";
            approveRequest.setManagerMotivation(expectedManagerMotivation);
            approveRequest.setVacationApproved(Boolean.FALSE.toString());

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    STRUCTURED_VARIABLE_SU, OPERATION_APPROVE, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    approveRequest, MARSHALLER);

            final ResponseMessage response = COMPONENT.sendAndGetResponse(request, new ServiceProviderImplementation() {

                @Override
                public Message provides(final RequestMessage alarmRequestMsg) throws Exception {
                    // Assert the 1st request sent by Flowable on orchestrated service
                    final MessageExchange alarmMessageExchange = alarmRequestMsg.getMessageExchange();
                    assertEquals(ALARM_INTERFACE, alarmMessageExchange.getInterfaceName());
                    assertEquals(ALARM_SERVICE, alarmMessageExchange.getService());
                    assertNotNull(alarmMessageExchange.getEndpoint());
                    assertEquals(ALARM_ENDPOINT, alarmMessageExchange.getEndpoint().getEndpointName());
                    assertEquals(ALARM_CREATE_OPERATION, alarmMessageExchange.getOperation());
                    assertEquals(alarmMessageExchange.getStatus(), ExchangeStatus.ACTIVE);
                    final Object alarmCreateRequestObj = UNMARSHALLER.unmarshal(alarmRequestMsg.getPayload());
                    assertInstanceOf(Create.class, alarmCreateRequestObj);
                    final Create alaramCreateRequest = (Create) alarmCreateRequestObj;
                    assertEquals(vacationDate, alaramCreateRequest.getDate());

                    // Returns the reply of the service provider to the Flowable service task
                    return new StatusToConsumerMessage(alarmRequestMsg, ExchangeStatus.DONE);
                }

                @Override
                public boolean statusExpected() {
                    return false;
                }
            });

            final Source fault = response.getFault();
            assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
            assertNotNull(response.getPayload(), "No XML payload in response");
            final Object responseObj = UNMARSHALLER.unmarshal(response.getPayload());
            assertInstanceOf(AckResponse.class, responseObj);

            // Assert that structured variable is correctly set
            final ProcessInstance procInst = FLOWABLE_CLIENT.getRuntimeService().createProcessInstanceQuery()
                    .processInstanceId(processInstanceId.toString()).includeProcessVariables().singleResult();
            assertNotNull(procInst);
            final Object variableObj = procInst.getProcessVariables().get("approvementStatus");
            assertInstanceOf(JsonNode.class, variableObj);
            final JsonNode variableJson = (JsonNode) variableObj;
            assertNotNull(variableJson.get("vacationApproved"));
            assertFalse(variableJson.get("vacationApproved").asBoolean());
            assertNotNull(variableJson.get("managerMotivation"));
            assertEquals(expectedManagerMotivation, variableJson.get("managerMotivation").asText());
        }

        this.assertUserTaskEnded(processInstanceId.toString(), USER_TASK, BPMN_USER);

        // Wait message event subscription
        this.waitIntermediateCatchMessageEvent(processInstanceId.toString(), MESSAGE_EVENT_NAME);

        // ----------------------------------------------------------------------------
        // Send the intermediate message event when expected by the BPMN engine
        // ----------------------------------------------------------------------------
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstanceId.toString());
            unlockRequest.setVar1(VARIABLE_1_VALUE);
            unlockRequest.setVar2(VARIABLE_2_VALUE);

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    STRUCTURED_VARIABLE_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request);
            final StatusMessage response = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.DONE, response.getStatus());
        }

        // Wait the end of the process instance
        this.waitEndOfProcessInstance(processInstanceId.toString());

        // Assertions about state of process instance at Flowable Level
        this.assertProcessInstanceFinished(processInstanceId.toString());

        // Assert that complementary variables are correctly set
        final HistoricProcessInstance hisProcInst = FLOWABLE_CLIENT.getHistoryService()
                .createHistoricProcessInstanceQuery().processInstanceId(processInstanceId.toString())
                .includeProcessVariables().singleResult();
        assertNotNull(hisProcInst);
        assertEquals(VARIABLE_1_VALUE, hisProcInst.getProcessVariables().get("var1"));
        assertEquals(VARIABLE_2_VALUE, hisProcInst.getProcessVariables().get("var2"));

    }
}
