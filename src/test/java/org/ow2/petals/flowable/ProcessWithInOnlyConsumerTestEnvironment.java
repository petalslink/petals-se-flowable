/**
 * Copyright (c) 2017-2025 Linagora
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

import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * Abstract class for unit tests about process invoking InOnly or RobustInOnly services
 * 
 * @author Christophe DENEUX - Linagora
 */
public abstract class ProcessWithInOnlyConsumerTestEnvironment extends AbstractTestEnvironment {

    protected static final String INONLY_SU = "in-only";

    protected static final String INONLY_SU_HOME = "su/in-only/";

    private static final String INONLY_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/in-only";

    protected static final QName INONLY_INTERFACE = new QName(INONLY_NAMESPACE, "in-only");

    protected static final QName INONLY_SERVICE = new QName(INONLY_NAMESPACE, "in-only-service");

    protected static final String INONLY_ENDPOINT = "edpInOnly";

    protected static final QName OPERATION_START = new QName(INONLY_NAMESPACE, "start");

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/in-only/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "in-only";

    @BeforeAll
    private static void initJaxBTooling() throws JAXBException {
        final JAXBContext context = JAXBContext.newInstance(
                org.ow2.petals.se_flowable.unit_test.in_only.ObjectFactory.class,
                org.ow2.petals.se_flowable.unit_test.in_only.archivageservice.ObjectFactory.class);
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
        COMPONENT_UNDER_TEST.registerServiceToDeploy(INONLY_SU, new ServiceConfigurationFactory() {
            @Override
            public ServiceConfiguration create() {

                final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(INONLY_SU_HOME + "in-only.wsdl");
                assertNotNull(wsdlUrl, "WSDL not found");
                final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                        INONLY_INTERFACE, INONLY_SERVICE, INONLY_ENDPOINT, wsdlUrl);

                final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(INONLY_SU_HOME + "startResponse.xsl");
                assertNotNull(startResponseXslUrl, "Output XSL 'startResponse.xsl' not found");
                serviceConfiguration.addResource(startResponseXslUrl);

                final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(INONLY_SU_HOME + "in-only.bpmn");
                assertNotNull(bpmnUrl, "BPMN file not found");
                serviceConfiguration.addResource(bpmnUrl);

                final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(INONLY_SU_HOME + "archivageService.wsdl");
                assertNotNull(archivageServiceWsdlUrl, "archivageService WSDL not found");
                serviceConfiguration.addResource(archivageServiceWsdlUrl);

                serviceConfiguration.setServicesSectionParameter(
                        new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "in-only.bpmn");
                serviceConfiguration.setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"),
                        "1");

                // By default (according to process definition and WSDL), archiving service is invoke with MEP
                // 'InOnly'

                return serviceConfiguration;
            }
        }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE)
                .postInitComponentUnderTest();
    }
}
