/**
 * Copyright (c) 2014-2026 Linagora
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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_SERVICE;

import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * Test environement of unit tests based on multi-start processes
 * 
 * @author Christophe DENEUX - Linagora
 */
public abstract class MultistartProcessTestEnvironment extends AbstractTestEnvironment {

    protected static final String MULTISTART_SU = "multi-start-su";

    protected static final String MULTISTART_SU_HOME = "su/multi-start/";

    private static final String MULTISTART_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/multi-start";

    protected static final QName MULTISTART_INTERFACE = new QName(MULTISTART_NAMESPACE, "multistart");

    protected static final QName MULTISTART_SERVICE = new QName(MULTISTART_NAMESPACE, "multiStartService");

    protected static final String MULTISTART_ENDPOINT = "edpMultiStart";

    protected static final QName OPERATION_START_BY_WEB = new QName(MULTISTART_NAMESPACE, "start-by-web");

    protected static final QName OPERATION_START_BY_ONLINE_AGENT = new QName(MULTISTART_NAMESPACE,
            "start-by-online-agent");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "multi-start";

    protected static final String BPMN_USER = "kermit";

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/multi-start/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    private static final String CORE_SVC_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/multi-start/coreService";

    protected static final QName CORE_SVC_INTERFACE = new QName(CORE_SVC_NAMESPACE, "core");

    protected static final QName CORE_SVC_SERVICE = new QName(CORE_SVC_NAMESPACE, "coreService");

    protected static final String CORE_SVC_ENDPOINT = "coreServiceEndpointName";

    protected static final QName CORE_SVC_OPERATION = new QName(CORE_SVC_NAMESPACE, "execute");

    @BeforeAll
    private static void initJaxBTooling() throws JAXBException {
        final JAXBContext context = JAXBContext.newInstance(
                org.ow2.petals.components.flowable.generic._1.ObjectFactory.class,
                org.ow2.petals.se_flowable.unit_test.multi_start.ObjectFactory.class,
                org.ow2.petals.se_flowable.unit_test.multi_start.archivageservice.ObjectFactory.class,
                org.ow2.petals.se_flowable.unit_test.multi_start.coreservice.ObjectFactory.class);
        UNMARSHALLER = context.createUnmarshaller();
        MARSHALLER = context.createMarshaller();
        MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
    }

    @BeforeAll
    private static void completesTestEnvConfiguration() throws Exception {
        FLOWABLE_CLIENT.start();
        completesComponentUnderTestConfiguration();
    }

    private static void completesComponentUnderTestConfiguration() throws Exception {
        COMPONENT_UNDER_TEST.registerServiceToDeploy(MULTISTART_SU, new ServiceConfigurationFactory() {
            @Override
            public ServiceConfiguration create() {

                final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(MULTISTART_SU_HOME + "multi-start.wsdl");
                assertNotNull(wsdlUrl, "WSDL not found");
                final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                        MULTISTART_INTERFACE, MULTISTART_SERVICE, MULTISTART_ENDPOINT, wsdlUrl);

                final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(MULTISTART_SU_HOME + "startResponse.xsl");
                assertNotNull(startResponseXslUrl, "Output XSL 'startResponse.xsl' not found");
                serviceConfiguration.addResource(startResponseXslUrl);

                final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(MULTISTART_SU_HOME + "multi-start.bpmn");
                assertNotNull(bpmnUrl, "BPMN file not found");
                serviceConfiguration.addResource(bpmnUrl);

                final URL notifyServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(MULTISTART_SU_HOME + "notifyService.wsdl");
                assertNotNull(notifyServiceWsdlUrl, "notifyService WSDL not found");
                serviceConfiguration.addResource(notifyServiceWsdlUrl);

                final URL coreServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(MULTISTART_SU_HOME + "coreService.wsdl");
                assertNotNull(coreServiceWsdlUrl, "coreService WSDL not found");
                serviceConfiguration.addResource(coreServiceWsdlUrl);

                final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(MULTISTART_SU_HOME + "archivageService.wsdl");
                assertNotNull(archivageServiceWsdlUrl, "archivageService WSDL not found");
                serviceConfiguration.addResource(archivageServiceWsdlUrl);

                serviceConfiguration.setServicesSectionParameter(
                        new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "multi-start.bpmn");
                serviceConfiguration.setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"),
                        "1");

                // Consume service 'archiver'
                final ConsumesServiceConfiguration consumeServiceConfiguration = new ConsumesServiceConfiguration(
                        ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                serviceConfiguration.addServiceConfigurationDependency(consumeServiceConfiguration);

                return serviceConfiguration;
            }
        }).registerNativeServiceToDeploy(NATIVE_TASKS_SVC_CFG, new NativeServiceConfigurationFactory() {

            @Override
            public ServiceConfiguration create(final String nativeEndpointName) {

                final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("component.wsdl");
                assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                return new ProvidesServiceConfiguration(ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE, nativeEndpointName,
                        nativeServiceWsdlUrl);
            }

            @Override
            public QName getNativeService() {
                return ITG_TASK_SERVICE;
            }
        }).registerNativeServiceToDeploy(NATIVE_PROCESSINSTANCES_SVC_CFG, new NativeServiceConfigurationFactory() {

            @Override
            public ServiceConfiguration create(final String nativeEndpointName) {

                final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("component.wsdl");
                assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                return new ProvidesServiceConfiguration(ITG_PROCESSINSTANCES_PORT_TYPE, ITG_PROCESSINSTANCES_SERVICE,
                        nativeEndpointName, nativeServiceWsdlUrl);
            }

            @Override
            public QName getNativeService() {
                return ITG_PROCESSINSTANCES_SERVICE;
            }
        }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE)
                .registerExternalServiceProvider(CORE_SVC_ENDPOINT, CORE_SVC_SERVICE, CORE_SVC_INTERFACE)
                .postInitComponentUnderTest();
    }
}
